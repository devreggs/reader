package reader.book_processor

import java.io.{FileInputStream, OutputStreamWriter, File}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import net.liftweb.common.{Full, Box, Logger, Empty}
import net.liftweb.json
import net.liftweb.util._
import Helpers._
import java.nio.file.{StandardCopyOption, CopyOption, Files, Paths, Path}
import java.nio.charset.Charset
import org.xml.sax.SAXException

import scala.xml._
import org.zeroturnaround.zip.ZipUtil
import javax.imageio.ImageIO
import net.liftweb.common.Full
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import java.net.URI

import scala.xml.parsing.NoBindingFactoryAdapter

//import net.liftweb.json.JsonAST._
//import net.liftweb.json

/**
 * Created by IvanchukovaEV on 26.05.14.
 */
class EpubProcessor extends Logger{

    protected val ImageSizeConstant = 1000
    protected val nse = NodeSeq.Empty
    protected val zipFileSeparator = "/"

    // используются для генерации новых id
    protected var linkCounter: Int = 0
    protected val linkPrefix = "jetreader"

    protected def getNextLink = {
        linkCounter += 1
        linkPrefix + linkCounter
    }

    protected def getLastLink = linkPrefix + linkCounter

    // json описание сносок, длина которых меньше TipMaxSize
    protected var tips: List[JValue] = List()
    protected var TipMaxSize = 500

    protected val DefaultCssPrefix = "book"

    protected class procParams (val sourceFileFolder: String,
                                val binariesMap: Map[String, BinaryDescription])
    {

    }

    protected class PieDescription (val index: Int,
                                     val filename: String,
                                     val size: Int,
                                     val containedIds: List[String])
    {
        
    }

    protected class BinaryDescription (val sourceId: String,
                                        val newId: String,
                                        val sourceFilename: String,
                                        val newFilename: String,
                                        val imageWidth: Int,
                                        val imageAspectRatio: Double)
    {
        def this() = this("", "", "", "", 0, 1.0)
    }

    def getMainContainerFilename(epubFolder: String) = {
        val metainfFolder = epubFolder + File.separator + "META-INF"
        if (Files.notExists(Paths.get(metainfFolder)))
            ""
        else {
            new File(metainfFolder).listFiles().find { case directoryEntry => {
                if (!directoryEntry.isDirectory)
                    try {
                        (loadXML(directoryEntry.toString) \ "rootfiles" \ "rootfile" \ "@media-type" text) == "application/oebps-package+xml"
                    } catch { 
                        case _: Throwable => false
                    }
                else
                    false
                }
            }.map(_.getPath).getOrElse("")
        }
    }

    protected def getMetaProperty(epubMetadata: NodeSeq, element: Node, propertyName: String) = {
        val elementId = (element \ "@id" text)
        if (element.attribute(propertyName).isEmpty) {
            (epubMetadata \ "meta").find{ case p => {
                ((p \ "@refines" text) == s"#$elementId") &&
                    ((p \ "@property" text) == propertyName)
            }}.getOrElse(nse).text
        } else {
            element.attribute(propertyName).head.text
        }
    }

    protected def getMetaAttribute(epubMeta: NodeSeq, propertyName: String, attributeName: String) = {
        ((epubMeta \ "meta").find{ case p => {
            (p \ "@name" text) == propertyName
        }}.getOrElse(nse) \ s"@$attributeName").text
    }

    protected def getManifestItem(epubManifest: NodeSeq, itemId: String, attributeName: String) = {
        ((epubManifest \ "item").find{ case p => {
            (p \ "@id" text) == itemId
        }}.getOrElse(nse) \ s"@$attributeName").text
    }

    protected def parseDescription(epubOPF: NodeSeq, binariesMap: Map[String, BinaryDescription]) = {
        val result = new BookDescription()
        val description = epubOPF \ "metadata"
        if (!description.isEmpty){
            // извлекаем заголовки
            result.title = (description \ "title").find((p: Node) => {
                getMetaProperty(description, p, "title-type") == "main"
            }).getOrElse((description \ "title").head).text

            result.subtitles = (for (titleTag <- (description \ "title").filter((p: Node) => {
                getMetaProperty(description, p, "title-type") == "subtitle"
            })) yield {
                (getMetaProperty(description, titleTag, "display-seq"), titleTag.text)
            }).sortWith(comparePairsByKey).map(_._2).toList

            result.fullTitle = (description \ "title").find((p: Node) => {
                getMetaProperty(description, p, "title-type") == "extended"
            }).getOrElse(nse).text

            result.edition = (description \ "title").find((p: Node) => {
                getMetaProperty(description, p, "title-type") == "edition"
            }).getOrElse(nse).text

            // информация о серии книг
            result.sequence = (for (titleTag <- description \ "title" if
                getMetaProperty(description, titleTag, "title-type") == "collection")
            yield new Sequence(titleTag.text, getMetaProperty(description, titleTag, "group-position"))).toList

            // все подряд идентификаторы книги в одну строку
            result.id = (description \ "identifier").map { case idTag => {
                getMetaProperty(description, idTag, "identifier") + ": " + idTag.text
            }}.mkString("; ")


            // информация о языке книги
            result.language = (description \ "language").text

            // получем список creator-ов с явно заданной ролью автора (список дополняем ключами - значениями display-seq)
            var aut = (description \ "creator").filter{ case p =>
                getMetaProperty(description, p, "role") == "aut"
            }.map { case creatorTag =>
                (getMetaProperty(description, creatorTag, "display-seq"), new AuthorName(creatorTag.text))
            }
            // если ни один создатель не имеет роль автора, то получаем список создателей без роли
            if (aut.length == 0) {
                aut = aut ++: (description \ "creator").filter{ case p =>
                    getMetaProperty(description, p, "role").isEmpty
                }.map { case creatorTag =>
                    (getMetaProperty(description, creatorTag, "display-seq"), new AuthorName(creatorTag.text))
                }
            }

            // сортируем всех авторов в списке по значению display-seq (либо в порядке появления)
            result.authors = aut.sortWith(comparePairsByKey).map( _._2 ).toList

            result.description = (description \ "description").text
            result.date = (description \ "date").text
            result.publisher = (description \ "publisher").text

            //TODO: найти пример жанра

            //TODO: найти тип участника для переводчиков
            val fb2Translator = getMetaAttribute(description, "FB2.book-info.translator", "content")
            if (!fb2Translator.isEmpty)
                //result.translators = List(new AuthorName(fb2Translator.split(" "):_*))
                result.translators = List(new AuthorName(fb2Translator, "","","",""))

            // пытаемся найти всё остальное в fb2-свойствах
            result.publishTitle = getMetaAttribute(description, "FB2.publish-info.book-name", "content")

            result.publishCity = getMetaAttribute(description, "FB2.publish-info.city", "content")

            result.publishYear = getMetaAttribute(description, "FB2.publish-info.year", "content")

            // извлекаем картинку обложки
            val coverMetaId = getMetaAttribute(description, "cover", "content")
            if (!coverMetaId.isEmpty) {
                result.coverpage = binariesMap(coverMetaId).newFilename
            }
        }

        result
    }

    // функция сортировки пар по первому значению
    def comparePairsByKey(left: (String, Any), right: (String, Any)) = {
        if (left._1.isEmpty && !right._1.isEmpty)
            false
        else if (!left._1.isEmpty && !right._1.isEmpty)
            left._1.compareTo(right._1) < 0
        else
            true
    }

    // рекурсивное преобразование NCX-файла в html-страничку оглавления
    def tocFromNcx(navPointSeq: NodeSeq, sectionLevel: Int, ncxFilename: String): Seq[Node] = {
        // пакуем список точек навигации с playOrder в качестве ключа
        navPointSeq.map {
            case navPointTag => (navPointTag.head \ "@playOrder" text, navPointTag)
            // сортируем точки навигации по значению playOrder (либо в порядке появления)
        }.sortWith { case ((left, a1), (right, a2)) => {
            if (!left.isEmpty && !right.isEmpty)
                left.toInt <= right.toInt
            else
                true
            // на каждую точку навигации - <a .../>++<br/> и все вложенные точки
        }}.map { case (playOrder, navPointTag) => {
            val navPointSrc = Paths.get(ncxFilename).getParent + File.separator + (navPointTag \ "content" \ "@src" text).replace("/", File.separator)
            <a href="#" goto-contents={anchorsMap(navPointSrc)} class={"toc-level" + sectionLevel}>
                {navPointTag \ "navLabel" \ "text" text}
            </a> ++ <br/> ++ tocFromNcx(navPointTag \ "navPoint", sectionLevel + 1, ncxFilename).flatten
        }}.flatten
    }

    def getSupportedMediaTypes = List("application/xhtml+xml", "application/x-dtbook+xml", "text/x-oeb1-document")

    // разворачиваем fallback-и для элементов секции spine в OPF
    def unchainSpineItem(manifest: NodeSeq, itemId: String, chain: List[String]): NodeSeq = {
        if (chain.contains(itemId))
            throw new Exception(s"recursive manifest fallback detected (id chain: ${chain.mkString(" ")})")
        
        val manifestItem = (manifest \ "item").find((p: Node) => {(p \ "@id").text == itemId})
        if (manifestItem.isEmpty)
            throw new Exception(s"manifest reference to non-existent id (${itemId})")

        val mediaType = (manifestItem.get \ "@media-type").text
        if (getSupportedMediaTypes.contains(mediaType) && (manifestItem.get \ "@fallback").isEmpty && (manifestItem.get \ "@fallback-style").isEmpty )
            manifestItem.get
        else {
            // TODO: support of different media types
            if (!(manifestItem.get \ "@fallback").isEmpty) {
                unchainSpineItem(manifest, manifestItem.get \ "@fallback" text, chain :+ itemId)
            } else
                throw new Exception(s"unsupported media type ${manifestItem.get \ "@media-type" text} in manifest item ${itemId}")
        }
    }

    // мап ссылок на файлы и якоря
    protected var anchorsMap: Map[String, String] = Map()

    def processBody(ns: NodeSeq, params: procParams): NodeSeq = {

        def transformChain: NodeSeq => NodeSeq = {
            "*" #> ((tagSeq: NodeSeq) => {
                val result =
                    new Elem(null,
                        tagSeq.head.label,
                        tagSeq.head.attributes,
                        tagSeq.head.scope,
                        true,
                        tagSeq.head.child:_*)
                val id = (tagSeq.head \ "@id" text)
                if (!id.isEmpty) {
                    if (lengthOfBranch(result) <= TipMaxSize)
                        tips = tips :+ (("tip" -> id) ~ ("content" -> result.mkString))
                }
                result
            }) andThen
            "script" #> ((tagSeq: NodeSeq) => {
                // script выкидываем
                nse
            }) &
            "body" #> ((tagSeq: NodeSeq) => {
                // body просто заменяем на div
                new Elem(null,
                    "div",
                    tagSeq.head.attributes,
                    tagSeq.head.scope,
                    true,
                    processBody(tagSeq.head.child, params):_*) ++ Text("\r\n") ++ <div></div>
            }) &
            "svg" #> ((tagSeq: NodeSeq) => {
                //processBody(tagSeq.head.child, params)
                tagSeq.map { case tag => processBody(tag.child, params)}
            }) &
            "img" #> ((tagSeq: NodeSeq) => {
                // к картинке добавляем атрибуты mw и h
                val imageSrcRel = (tagSeq.head \ "@src" text).replace("/", File.separator)
                val imageSrc = params.sourceFileFolder + imageSrcRel

                val binary = params.binariesMap.get(imageSrc).getOrElse(new BinaryDescription())

                <img mw={binary.imageAspectRatio.toString} h={binary.imageWidth.toString} src={binary.newFilename}>{tagSeq.head.child}</img> % tagSeq.head.attributes.filter {case f => f.key != "src"}

            }) &
            "image" #> ((tagSeq: NodeSeq) => {
                // к картинке добавляем атрибуты mw и h
                val xlinkUri = "http://www.w3.org/1999/xlink"
                val xlinkPrefix = tagSeq.head.scope.getPrefix(xlinkUri)

                val svgSrcRel = (tagSeq.head.attribute(xlinkUri, "href").getOrElse(nse) text).replace("/", File.separator)
                val svgSrc = params.sourceFileFolder + svgSrcRel

                val fallbackImageSrcRel = (tagSeq.head \ "@src" text).replace("/", File.separator)
                val fallbackImageSrc = params.sourceFileFolder + fallbackImageSrcRel

                val binaryToTake = if (fallbackImageSrcRel.isEmpty || params.binariesMap.get(fallbackImageSrc).isEmpty)
                    params.binariesMap.get(svgSrc).getOrElse(new BinaryDescription()) else
                    params.binariesMap.get(fallbackImageSrc).getOrElse(new BinaryDescription())

                new Elem(tagSeq.head.prefix,
                    "img",
                    tagSeq.head.attributes.filter {
                        case f => !(f.prefixedKey == (xlinkPrefix + ":href") || f.key == "src" || f.prefixedKey.startsWith("xmlns"))
                    }.append(
                            Attribute("mw", Text(binaryToTake.imageAspectRatio.toString),
                                Attribute("h", Text(binaryToTake.imageWidth.toString),
                                    Attribute("src", Text(binaryToTake.newFilename), Null)
                                ))),
                    tagSeq.head.scope,
                    true,
                    processBody(tagSeq.head.child, params): _*
                )
            }) &
            "text" #> ((tagSeq: NodeSeq) => {
                <span class={DefaultCssPrefix + "cutline"}>{processBody(tagSeq.head.child, params)}</span>
            }) &
            "a" #> ((tagSeq: NodeSeq) => {
                val hrefRel = (tagSeq.head \ "@href" text)
                val href = params.sourceFileFolder + hrefRel
                val id = (tagSeq.head \ "@id").text

                if (hrefRel.isEmpty)
                    <a>{processBody(tagSeq.head.child, params)}</a> % tagSeq.head.attributes
                else if (anchorsMap.contains(href)) {
                    <a href="#" goto={anchorsMap(href)}>{processBody(tagSeq.head.child, params)}</a> % tagSeq.head.attributes.filter{case f => f.key != "href"}.append({
                        if (id.isEmpty) {
                            //val newId = getNextLink
                            if (tagSeq.text == "[2]")
                                debug ("id added1 " + getNextLink)

                            // добавить в карту ссылок нечего, ибо не было здесь id, а значит, не было и возможности придти сюда по ссылке
                            //anchorsMap = anchorsMap + ((filename + "#" + anchor, getLastLink))
                            Attribute("id", Text(getLastLink), Null)
                        } else {
                            if (tagSeq.text == "[2]")
                                debug ("i've seen you before1")

                            Null
                        }
                    })
                }
                else
                    <a target="_blank">{processBody(tagSeq.head.child, params)}</a> % tagSeq.head.attributes
            })
        }

        if (ns.isEmpty)
            ns
        else
            transformChain(ns)
    }

    protected def replaceIdsAndAnchors(input: NodeSeq, filename: String): NodeSeq = {

        def transformChain: NodeSeq => NodeSeq = {
            "*" #> ((tagSeq: NodeSeq) => {
                // у всех элементов, имеющих "родной" id, надо проставить наш фирменный id
                if (tagSeq.text == "[2]")
                    debug ("replaceIdsAndAnchors *")
                val id = (tagSeq.head \ "@id").text
                val attributes = tagSeq.head.attributes.filter{ case f => f.key != "id"
                }.append(
                    if (id.nonEmpty && !id.startsWith(linkPrefix)) {
                        // добавляем в карту ссылок
                        anchorsMap = anchorsMap + ((filename + "#" + id, getNextLink))
                        if (tagSeq.text == "[2]")
                            debug ("id replaced " + getLastLink)
                        Attribute("id", Text(getLastLink), Null)
                    } else
                        Null
                )

                new Elem(null,
                    tagSeq.head.label,
                    attributes,
                    tagSeq.head.scope,
                    true,
                    replaceIdsAndAnchors(tagSeq.head.child, filename):_*)
            }) andThen
            "a" #> ((tagSeq: NodeSeq) =>{
                // здесь нам надо заменить на фирменный id имя ссылки, заданное атрибутом name; заменять/добавлять id не нужноб рекурсивно обходить дочерние элементы -
                // тоже не нужно, т.к. это уже сделано предыдущим селектором
                val anchor = (tagSeq.head \ "@name" text)


                <a>{tagSeq.head.child}</a> % tagSeq.head.attributes.filter{case f => f.key != "name"}.append({
                    if (anchor.nonEmpty) {
                        // добавляем в карту ссылок
                        anchorsMap = anchorsMap + ((filename + "#" + anchor, getNextLink))
                        Attribute("id", Text(getLastLink), Null)
                    } else
                        Null
                })
            })
        }

        if (input.isEmpty)
            input
        else
            transformChain(input)
    }

    protected def lengthOfBranch(input: NodeSeq) =
        input.mkString.length + (input \\ "img").length * ImageSizeConstant
        // input.mkString.length + (for (nestedTag <- input \\ "img") yield ImageSizeConstant).sum

    def flushFile(targetPath: Path, fileContent: String) {
        val writer = Files.newBufferedWriter(targetPath, Charset.forName("UTF-8"))
        writer.write(fileContent)
        writer.flush()
        writer.close()
    }

    def cleanUpDirectory(directory: File): Unit = {
        if (directory.isDirectory)
            for (directoryEntry <- directory.listFiles())
                if (directoryEntry.isDirectory)
                    cleanUpDirectory(directoryEntry)
                else
                    directoryEntry.delete()
        directory.delete()
    }

    def loadXML(input: String): NodeSeq = {
        val factory: DocumentBuilderFactory  = DocumentBuilderFactory.newInstance()
        factory.setNamespaceAware(true)
        factory.setValidating(false)
        val documentBuilder: DocumentBuilder  = factory.newDocumentBuilder()
        documentBuilder.setEntityResolver(new EntityManager())



        val dom2sax = new com.sun.org.apache.xalan.internal.xsltc.trax.DOM2SAX(documentBuilder.parse(new File(input)))
        val adapter = new NoBindingFactoryAdapter
        dom2sax.setContentHandler(adapter)
        dom2sax.parse()

        adapter.rootElem
    }

    class EntityManager extends EntityResolver {
        def resolveEntity(publicId: String, systemId: String ): InputSource = {
            /* code goes here to return contents of DTD */
            new InputSource(systemId)
        }

    }

    def convertToHtml(fullPath: String, output: String, pieMaxSize: Int, tipMaxSize: Int, srcPrefix: String): Box[BookDescription] = {

        try {
            val outputPath = Paths.get(output)
            if (Files.exists(outputPath))
                for (oldFilename <- new File(output).listFiles()) {
                    if (oldFilename.getName != new File(fullPath).getName)
                        if (oldFilename.isDirectory)
                            cleanUpDirectory(oldFilename)
                        else
                            oldFilename.delete()
                }
            else
                Files.createDirectory(outputPath)

            val outputFolder = output + File.separator
            val outputFolderUri = new File(outputFolder).toURI

            val unzippedFolder = outputFolder + "src"

            if (Files.exists(Paths.get(unzippedFolder)))
                cleanUpDirectory(new File(unzippedFolder))
            else
                Files.createDirectory(Paths.get(unzippedFolder))

            ZipUtil.unpack(new File(fullPath), new File(unzippedFolder))

            val containerFilename = getMainContainerFilename(unzippedFolder)
            if (!containerFilename.isEmpty) {
                val container = scala.xml.XML.loadFile(containerFilename)
                val opfRelPath = (container \\ "rootfile"  \ "@full-path" text)
                val OPF = scala.xml.XML.loadFile(unzippedFolder + File.separator + opfRelPath)
                val opfFolderPath = Paths.get(unzippedFolder + File.separator + opfRelPath).getParent
                val opfFolderUri = opfFolderPath.toUri

                // Перемещаем файлики в корень
                val binariesMap = (for ((binaryTag, index) <- (OPF \ "manifest" \ "item").zipWithIndex if (!getSupportedMediaTypes.contains(binaryTag \ "@media-type" text))) yield {
                    val sourceHref = (binaryTag \ "@href" text)
                    val mediaType = (binaryTag \ "@media-type" text)
                    val sourceFilename = outputFolder + outputFolderUri.relativize(new File(opfFolderPath + File.separator + sourceHref).toURI).toString.replace("/", File.separator)

                    val imageAttrs =
                        if (mediaType.startsWith("image/")) {
                            val image = ImageIO.read(new File(sourceFilename))
                            // картинки переименовываем по номерам
                            (index.toString + "." + mediaType.substring(mediaType.lastIndexOf("/") + 1), image.getWidth, image.getHeight * 1.0 / image.getWidth)
                        } else
                            // для всех остальных файлов сохраняем исходное имя
                            (sourceHref.substring(sourceHref.lastIndexOf("/") + 1), 0, 0.0)

                    Files.copy(Paths.get(sourceFilename), Paths.get(outputFolder + imageAttrs._1), StandardCopyOption.REPLACE_EXISTING)
                    // пишем имена файлов в таблицу якорей
                    anchorsMap = anchorsMap + ((sourceFilename, outputFolder + imageAttrs._1))

                    (binaryTag \ "@id" text,
                        new BinaryDescription(
                            binaryTag \ "@id" text,
                            getNextLink,
                            sourceFilename.replace(outputFolder, ""),
                            imageAttrs._1,
                            imageAttrs._2,
                            imageAttrs._3
                        ))
                }).toMap

                val binariesBySourceFilenameMap = binariesMap.view.map { case (id, binary) => {
                    (binary.sourceFilename, binary)
                }}.toMap

                // итерируем по паям: разыменовываем атрибут idref каждого itemref в секции spine в порядке
                // прямого чтения (если атрибут linear не указан или равен yes), затем в порядке появления
                val piesMap = (OPF \ "spine" \ "itemref").sortWith(
                    (left: Node, right: Node) => {
                        (left \ "@linear").text == "no" && ((right \ "@linear").isEmpty || (right \ "@linear").text == "yes")
                    }).zipWithIndex.map {
                    case (spineItem, pieIndex) => {
                        val manifestItem = unchainSpineItem(OPF \ "manifest", spineItem \ "@idref" text, List())
                        val pieRelFilename = opfFolderPath.toString + File.separator + (manifestItem \ "@href" text)
                        // pieFilename нам понадобится для замены всех содержащихся в пае ссылок на другие файлы,
                        // якоря и id, поэтому приводим его относительно выходного каталога (как в anchorsMap)
                        val pieFilename = outputFolderUri.relativize(Paths.get(pieRelFilename).toUri).toString.replace("/", File.separator)

                        (replaceIdsAndAnchors(loadXML(pieRelFilename), pieFilename), pieFilename, pieIndex)
                    }
                        // теперь, когда известна карта ссылок с изменёнными путями, итерируем по паям ещё раз,
                        // чтобы распарсить каждый из них
                }.map {
                    case (pie, pieSourceFilename, pieIndex) => {
                        // pieNewFilename нам понадобится для того, чтобы сохранить в тот же каталог файл
                        // pieNewFilename_head, в котором будет содержимое секции <head> этого пая,
                        // а также чтобы подменять пути к картинкам и другие ссылки
                        val pieNewFilename = outputFolder + pieIndex.toString + ".pie"
                        val pieProcessed = processBody(pie, new procParams(Paths.get(pieSourceFilename).getParent + File.separator, binariesBySourceFilenameMap))

                        flushFile(Paths.get(pieNewFilename + "_head"),
                            (pieProcessed \ "head").mkString)
                        val pieBody = pieProcessed \ "div"

                        flushFile(Paths.get(pieNewFilename),
                            pieBody.mkString)


                        // пишем имена файлов в таблицу якорей
                        anchorsMap = anchorsMap + ((pieSourceFilename, pieBody \ "div" \ "@id" text))

                        new PieDescription(pieIndex,
                            pieNewFilename,
                            lengthOfBranch(pieBody),
                            (for (nestedTag <- pieBody \\ "_" if (!nestedTag.attribute("id").isEmpty)) yield (nestedTag \ "@id" text)).toList)
                    }
                }

                val totalSize = piesMap.foldLeft[Double](0.0)( (sum: Double, p: PieDescription) => sum + p.size )

                val description = JObject(List(
                    JField("pages", JArray(
                        piesMap.zip(piesMap.scanLeft(0)( (sum, p) => sum + p.size ).tail).map {
                            case (pie, pieMax) => {
                                ("number" -> pie.index) ~
                                    ("pie" -> pie.size / totalSize) ~
                                    ("pieMin" -> (pieMax - pie.size) / totalSize) ~
                                    ("pieMax" -> pieMax / totalSize) ~
                                    ("ids" -> pie.containedIds)
                            }
                        }.toList)),
                    JField("tips", JArray(tips))
                ))

                flushFile(Paths.get(outputFolder + "description.json"),
                    json.Printer.pretty(render(description)))

                // id файла с таблицей содержимого (NCX-файл)
                val ncxMetaId = (OPF \ "spine" \ "@toc").text
                // путь к этому файлу
                val ncxFilename = opfFolderPath + File.separator + ((OPF \ "manifest" \ "item").find((p: Node) => {ncxMetaId == (p \"@id" text)}).getOrElse(nse) \ "@href").text

                // прогружаем таблицу содержания
                val NCX = loadXML(ncxFilename)
                // преобразуем её в html-страничку
                val toc = tocFromNcx(NCX \ "navMap" \ "_", 1, outputFolderUri.relativize(Paths.get(ncxFilename).toUri).toString.replace("/", File.separator))

                flushFile(Paths.get(outputFolder + "contents.html"),
                    toc.mkString)

                Full(parseDescription(OPF, binariesMap))
            } else {
                info(fullPath + ": not valid epub")
                Empty
            }
        } catch {
            case e: Exception =>
            info(fullPath + ": broken epub\r\n" + e.getMessage)
            debug(e.getStackTraceString)
            Empty
        }
    }
}
