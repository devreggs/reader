package reader.lib

import net.liftweb.http.FileParamHolder
import reader.model._
import org.zeroturnaround.zip.{ZipEntryCallback, ZipUtil}
import scala.actors.Actor._
import scala.actors.Actor
import java.io.{InputStream, File}
import scala.collection.JavaConversions._
import net.liftweb.common.Logger
import java.util.zip.{ZipEntry, ZipFile}
import reader.book_processor.{BookDescription, EpubProcessor, Fb2Processor}
import HelpSnippets._
import reader.comet.{UpdateUserBooks, ProcessingLobby}
import java.util.concurrent.LinkedBlockingQueue
import java.awt.image.BufferedImage
import java.awt.{Image, AlphaComposite}
import javax.imageio.{IIOImage, ImageWriteParam, ImageWriter, ImageIO}
import javax.imageio.stream.FileImageOutputStream
import com.mortennobel.imagescaling.{AdvancedResizeOp, ResampleFilters, ResampleOp}
import java.net.URL
import org.apache.commons.io.{FileUtils, FilenameUtils}
import java.nio.charset.Charset

object BookProcessor extends Logger {
    def removeBook(book: Book) = {
        info(s"delete book ${book.id.get}")
        if(SecurityGateway.allowRemove(book)) {
            book.delete_!
            User.setCurrentSessionMessage("Книга \"" + book.title.get + "\" удалена")
        }
        User.currentUser.books.refresh
        if (bookQueue.contains(book))
            bookQueue.remove(book)
        FileUtils.deleteDirectory(new File(book.directoryPath))
        book.delete_!
    }


    // Если перезагрузка сервера то добавить из базы все необработанные книги в очередь
    def init = Book.raw.foreach(pushBook(_))

    def reprocessAll = {
        info("reprocessAll")
        Author.bulkDelete_!!()
        Genre.bulkDelete_!!()

        Book.findAll().foreach(b => {
            if(b.state.get != BookState.raw) reprocessBook(b)
        })
    }

    def reprocessBook(book: Book) ={
        info(s"reprocess ${book.id.get}")
        book.authors.clear()
        book.genres.clear()
        book.failedMessage("")
        book.state(BookState.raw).saveMe()

        new File(book.directoryPath).listFiles().filter(_.getName != "original").foreach(file => {
            if(file.isDirectory)
                FileUtils.deleteDirectory(file)
            else
                file.delete()
        })
        pushBook(book)

    }

    def removeAllWrongs = {
        info("removeAllWrongs")
        Book.wrong.foreach(b => {
            val owner = b.owner.get
            b.delete_!
            ProcessingLobby ! UpdateUserBooks(owner)
        })
    }

    def recalcAuthorTitles = {
        info("recalcAuthorTitles")
        Book.findAll().foreach(book => {
            book.authorTitle(if(book.authors.size > 1)
                book.authors.map(_.shortNameLF.get).mkString(", ")
            else if(book.authors.size == 1)
                book.authors.head.fullNameLF.get
            else
                "").saveMe()
        })
    }

    // Обработчик книги
    protected class BookProcessorActor(val book: Book) extends Actor {
        def owner = book.owner.foreign.get

        case class BookProcessorException(val msg: String, val userMessage: String) extends Exception(msg)

        def act() = {
            try {
                ProcessingLobby ! UpdateUserBooks(book.owner.get)
                info("Begin process " + book.originalFileName.get + " of " + book.owner.foreign.get.email)

                // Загрузка url
                if(book.fromURL.get) {
                    owner.mutex.synchronized {
                        if(owner.freeSpace < 1)
                            throw new BookProcessorException("User has not free space", "Недостаточно свободного места")

                        try {
                            book.downloadFile("original", book.originalFileName.get)
                        } catch {
                            case _ => throw new BookProcessorException("Wrong URL", "Не удалось загрузить")
                        }

                        val stupidConnection = (new URL(book.originalFileName.get)).openConnection()
                        val header = stupidConnection.getHeaderField("Content-Disposition")
                        val filenameOptionDecoded = (if(header != null && header.contains("filename="))
                            header.split(';').map(_.trim).find(_.startsWith("filename="))
                                else
                            Some(FilenameUtils.getName(stupidConnection.getURL.toString)))

                        val ptext = filenameOptionDecoded.getOrElse("filename=original").getBytes("ISO-8859-1")
                        val filename = new String(ptext, "cp1251")

                        book.originalFileName(filename.drop(9).filter(c => (c != '"' && c != '=')).trim)
                        book.fromURL(false)
                        book.saveMe()

                        if(owner.freeSpace < 1)
                            throw new BookProcessorException("User has not free space", "Недостаточно свободного места")
                    }
                }

                FilenameUtils.getExtension(book.originalFileName.get) match {
                    case "fb2" => processFb2
                    case "epub" => processEpub
                    case "zip" => unzip
                    case _ => throw new BookProcessorException("Unknown extension", "Неизвестное расширение")
                }
                

            } catch {
                case e: BookProcessorException => {
                    error(e.msg)
                    error(e.getMessage)
                    error(e.getStackTrace.mkString("\n"))
                    failBook(e.userMessage)
                }
                case e: Exception => {
                    error(e.getMessage)
                    error(e.getStackTrace.mkString("\n"))
                    failBook("Не удалось обработать книгу")
                }
            } finally {
                ProcessingLobby ! UpdateUserBooks(book.owner.get)
                exit()
            }
        }

        def failBook(msg: String) = {
            warn("Fail process " + book.originalFileName.get + " of " + book.owner.foreign.get.email + ": " + msg)
            book.failedMessage(msg)
                .state(BookState.wrong)
                .saveMe()
            exit()
        }

        def writeJpeg(image: BufferedImage, destPath: String, quality: Float) {
            val writer = ImageIO.getImageWritersByFormatName("jpeg").next()
            val param = writer.getDefaultWriteParam()
            param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
            param.setCompressionQuality(quality)
            val output = new FileImageOutputStream(new File(destPath))
            writer.setOutput(output)
            val iioImage = new IIOImage(image, null, null)
            writer.write(null, iioImage, param)
            writer.dispose()
            output.close()
        }


        def makeThumbnails() {
            info("Make thumbnails " + book.originalFileName.get + " of " + book.owner.foreign.get.email + ": ")
            val originalImage = ImageIO.read(new File(book.filePath("parsed/" + book.coverPageFileName.get)))

            val resampleOpTile = new ResampleOp(120, 180)
            resampleOpTile.setFilter(ResampleFilters.getLanczos3Filter())
            //resampleOpTile.setUnsharpenMask(AdvancedResizeOp.UnsharpenMask.VerySharp)

            val relation = originalImage.getHeight.toDouble / originalImage.getWidth.toDouble
            val resampleOpView = new ResampleOp(150, Math.round(150.0 * relation).toInt)
            resampleOpView.setFilter(ResampleFilters.getLanczos3Filter())
            //resampleOpView.setUnsharpenMask(AdvancedResizeOp.UnsharpenMask.Soft)

            writeJpeg(resampleOpTile.filter(originalImage, null), book.filePath("parsed/thumb-tile.jpg"), 0.9f);
            writeJpeg(resampleOpView.filter(originalImage, null), book.filePath("parsed/thumb-view.jpg"), 0.9f);
        }

        def unzip = {
            /*ZipUtil.iterate(new File(book.filePath("original")), new ZipEntryCallback {
                override def process(p1: InputStream, p2: ZipEntry): Unit = {
                    if(!p2.isDirectory) {
                        saveInputStreamBook(p1,
                            p2.getName.split('/').last,
                            book.owner.foreign.openOrThrowException("Book without owner"))
                    }
                }
            })*/
            val charset = Charset.forName("Cp866");
            val zip = new ZipFile(new File(book.filePath("original")), charset)
            val zipEntries = zip.entries()

            while(zipEntries.hasMoreElements){
                val zipEntry = zipEntries.nextElement()
                if(!zipEntry.isDirectory) {
                    saveInputStreamBook(zip.getInputStream(zipEntry),
                        zipEntry.getName.split('/').last,
                        book.owner.foreign.openOrThrowException("Book without owner"))
                }
            }
            zip.close()
            book.delete_!
        }

        def processFb2 = {
            val bookDescBox = (new Fb2Processor()).convertToHtml(book.filePath("original"),
                book.filePath("parsed"),
                10000, 300, "", false)
            if(bookDescBox.isEmpty) throw new Exception("Failed to process fb2")
            saveBookDesc(bookDescBox.get)
        }

        def processEpub = {
            val bookDescBox = (new EpubProcessor()).convertToHtml(book.filePath("original"),
                book.filePath("parsed"),
                10000, 300, "")
            if(bookDescBox.isEmpty) throw new Exception("Failed to process epub")
            saveBookDesc(bookDescBox.get)
        }

        def saveBookDesc(bookDesc: BookDescription) = {
            val newBook = book.title(bookDesc.title.digest(Book.title.maxLen))
                .description(bookDesc.description.digest(Book.description.maxLen))
                .coverPageFileName(bookDesc.coverpage)
                .state(BookState.individual)
                .saveMe()

            val user = newBook.owner.foreign.openOrThrowException("Empty user box")
            user.books += newBook
            user.saveMe()
            
            for(author <- bookDesc.authors) {
                val fullNameFL = "%s %s %s".format(author.firstName,
                    author.middleName,
                    author.lastName).nameNormalize

                val fullNameLF = "%s %s %s".format(author.lastName,
                    author.firstName,
                    author.middleName).nameNormalize
                
                val shortNameFL = "%s %s".format((author.firstName + " " + author.middleName).nameHead,
                    author.lastName.nameCapitalize)

                val shortNameLF = "%s %s".format(author.lastName.nameCapitalize,
                    (author.firstName + " " + author.middleName).nameHead)

                val presentAuthor =( Author.byName(fullNameFL) or
                    Author.byName(fullNameLF) or
                    Author.byName(shortNameFL) or
                    Author.byName(shortNameLF)).openOr(Author.create
                    .email(author.email.toLowerCase.digest(Author.email.maxLen))
                    .nickname(author.nickname.digest(Author.nickname.maxLen))
                    .fullNameFL(fullNameFL)
                    .fullNameLF(fullNameLF)
                    .shortNameFL(shortNameFL)
                    .shortNameLF(shortNameLF)
                    .saveMe())
                newBook.authors += presentAuthor
            }
            newBook.saveMe()

            newBook.authorTitle(if(newBook.authors.size > 1)
                newBook.authors.map(_.shortNameLF).mkString(", ")
            else if(newBook.authors.size == 1)
                newBook.authors.head.fullNameLF.get
            else
                "").saveMe()
            

            for(genre <- bookDesc.genres) {
                val title = genre.toLowerCase.digest(Genre.title.maxLen)
                val presentGenre = Genre.bySourceTitle(genre).openOr(Genre.create.sourceTitle(title)
                    .title(title).saveMe())
                newBook.genres += presentGenre
            }
            newBook.saveMe()

            // Простительно невыполниться
            try {
                makeThumbnails()
            } catch {
                case e: Exception => {
                    error(e.getStackTrace.mkString("\n"))
                    error(e.getMessage)
                }
            }
        }
    }

    // Забирает из очереди книгу на обработку и создает для нее BookProcessorActor
    protected val bookProcessorManager = actor {
        var processActors = List[BookProcessorActor]()
        loop {
            processActors = processActors.filter(_.getState != Actor.State.Terminated)
            if(processActors.size < Environment.maxThreads && !bookQueue.isEmpty){
                info("New book processor")
                val newProcessor = new BookProcessorActor(popBook)
                processActors ::= newProcessor
                newProcessor.start()
            }
            Thread.sleep(1000)
        }
    }

    // Очередь книг на обработку
    // @volatile protected var bookQueue: Queue[Book] = Queue.empty
    protected val bookQueue = new LinkedBlockingQueue[Book]()

    protected def pushBook(book: Book) {
        bookQueue.put(book)
        info("Book push " + book.originalFileName.get + " of " + book.owner.foreign.get.email)
    }

    protected def popBook = {
        val book: Book = bookQueue.take()
        info("Book pop " + book.originalFileName.get + " of " + book.owner.foreign.get.email)
        book
    }

    // С этой или функции начинается загрузка новой книги если файл с ней был загружен на сервер
    def saveUploadedBook(param: FileParamHolder, user: User) = {
        val newBook = Book.create.owner(user).originalFileName(param.fileName).saveMe()
        newBook.uploadFile("original", param)
        pushBook(newBook)
    }

    // С этой или функции начинается загрузка новой книги если файл с
    // книгой был передан в виде ссылки на внешний ресурс
    def saveUrlBook(url: String, user: User) = {
        val newBook = Book.create.owner(user).originalFileName(url).fromURL(true).saveMe()
        pushBook(newBook)
    }

    def saveInputStreamBook(stream: InputStream, origFilename: String, user: User) ={
        val newBook = Book.create.owner(user).originalFileName(origFilename).saveMe()
        newBook.saveFileInputStream("original", origFilename, stream)
        pushBook(newBook)
    }
}
