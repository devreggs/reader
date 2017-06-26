package reader.lib

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import Helpers._
import reader.model._
import reader.lib.HelpSnippets._

import rest._
import net.liftweb.json._
import JsonDSL._
import java.io.{FileInputStream, File}
import org.apache.http.client.methods.HttpGet

object BookRestService extends RestHelper with Logger {

    serve {
        case "book" :: AsInt(bookId) :: content :: Nil Get req => stdResponse(req){
            val book = Book.byId(bookId).get
            val suffix = req.path.suffix
            val fileName = "parsed/" + content +
                (if(suffix != null && suffix.nonEmpty) "." + suffix else "")
            require(SecurityGateway.allowRead(book), "No rights")
            require(book.fileExist(fileName), "File not exists: " + fileName)

            val file = new File(book.filePath(fileName))
            val stream = new FileInputStream(file)
            val length = file.length()

            StreamingResponse(stream,
                () => stream.close(),
                length,
                ("Content-type" -> "application/octet-stream") :: ("Cache-Control" -> "max-age=7200") :: Nil,
                Nil,
                200)
        }

        case "download-original" :: AsInt(bookId) :: Nil Get req => stdResponse(req){
            val book = Book.byId(bookId).get

            require(SecurityGateway.allowRead(book), "No rights")
            require(book.fileExist("original"), "File not exists: original")

            val file = new File(book.filePath("original"))
            val stream = new FileInputStream(file)
            val length = file.length()

            StreamingResponse(stream,
                () => stream.close(),
                length,
                ("Content-type" -> "application/octet-stream") ::
                    ("Cache-Control" -> "max-age=7200") ::
                    ("filename" -> book.originalFileName.get) :: Nil,
                Nil,
                200)
        }

        case "upload" :: "book" :: Nil Post req => stdResponse(req){
            require(SecurityGateway.allowUpload, "No rights")
            val filesJson: List[JObject] = req.uploadedFiles.map(fph => {
                User.currentUser.mutex.synchronized {
                    if(fph.length <= User.currentUser.freeSpace) {
                        BookProcessor.saveUploadedBook(fph, User.currentUser)
                        ("name" -> fph.fileName) ~
                            ("size" -> fph.length) ~
                            ("length" -> fph.length) ~
                            ("delete_url" -> "") ~
                            ("delete_type" -> "DELETE")
                    } else {
                        ("error" -> "Нет места для новых книг") ~ Nil
                    }
                }
            })
            JsonResponse("files" -> filesJson)
        }

        case "upload-url" :: "book" :: Nil Post req => stdResponse(req){
            require(SecurityGateway.allowUpload, "No rights")
            BookProcessor.saveUrlBook(S.param("url").openOr(""), User.currentUser)
            OkResponse()
        }

        case "remove" :: "book" :: AsInt(bookId) :: Nil Get req => stdResponse(req){
            val book = Book.byId(bookId).get
            if(SecurityGateway.allowRemove(book)) {
                BookProcessor.removeBook(book)
                User.setCurrentSessionMessage("Книга \"" + book.title.get + "\" удалена")
                User.currentUser.books.refresh
            }
            RedirectResponse("/index")
        }

        case "set-position" :: "book" :: AsInt(bookId) :: Nil Post req => stdResponse(req){
            val book = Book.byId(bookId).get
            require(SecurityGateway.allowRead(book), "No rights")
            BookLastPosition.get(User.currentUser, book)
                .position(req.param("position").openOr("-1").toDoubleOr(0.0))
                .saveMe()
            OkResponse()
        }

        case "set-my-options" :: Nil Post req => stdResponse(req){
            val user = User.currentUser
            user.fontSize(S.param("font-size").openOr(user.fontSize.get))
                .fontFamily(S.param("font-family").openOr(user.fontFamily.get))
                .textAlign(S.param("text-align").openOr(user.textAlign.get))
                .theme(S.param("theme").openOr(user.theme.get))
                .saveMe()
            OkResponse()
        }

        case "get-my-free-space" :: Nil Get req => stdResponse(req){
            JsonResponse("result" -> User.currentUser.freeSpace)
        }

        case "translate" :: Nil Post req => stdResponse(req){
            val text = S.param("text").openOr("")
            val wordTranslation = LanguageTools.translateWord(text)
            val translation = (if(wordTranslation.isEmpty)
                LanguageTools.translateText(text) +
                    "<br><br><a style='font-style: italic; float: right; outline:none' href='http://translate.yandex.ru/'>Переведено сервисом \"Яндекс.Перевод\"</a>"
            else
                wordTranslation.mkString("<br>") +
                    "<br><br><a style='font-style: italic; float: right; outline:none' href='http://api.yandex.ru/dictionary/'>Реализовано с помощью сервиса \"Яндекс.Словарь\"</a>")
            JsonResponse("result" -> translation)
        }
    }
}
