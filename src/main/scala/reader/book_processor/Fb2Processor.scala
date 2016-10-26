package reader.book_processor

import java.io._
import sun.misc.BASE64Decoder
import net.liftweb.util._
import Helpers._
import scala.xml._
import scala.collection.mutable.{ListBuffer, HashMap}
import java.nio.file.{Paths, Files, Path}
import java.nio.charset.Charset
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import scala.collection.mutable
import net.liftweb.json
import net.liftweb.common.{Logger, Box, Full, Empty}
import javax.imageio.{ImageReader, ImageIO}


class Fb2Processor extends Logger{

    protected val ImageSizeConstant = 1000
    protected val linkPrefix = "intelgame"
    protected val DefaultCssPrefix = "book"
    protected val SectionLevelToDivide = 2 // 0 - книга, 1 - часть, 2 - глава

    protected var PieMaxSize = 10000
    protected var TipMaxSize = 500

    protected val nse = NodeSeq.Empty

    protected class procParams(val binariesMap: Map[String, (String, Int, Double)],
                               val sectionLevel: Int,
                               val headerCount: Int,
                               val bodyName: String)
    {
        def apply(binariesMap: Map[String, (String, Int, Double)] = null,
                  cssPrefix: String = null,
                  sectionLevel: Int = -1,
                  headerCount: Int = -1,
                  bodyName: String = null) =
            new procParams(if (binariesMap == null) this.binariesMap else binariesMap,
                if (sectionLevel == -1) this.sectionLevel else if (sectionLevel > 5) 5 else sectionLevel,
                if (headerCount == -1) this.headerCount else headerCount,
                if (bodyName == null) this.bodyName else bodyName
            )
    }

    // used for generating ids
    protected var linkCounter: Int = 0
    // table of contents (toc) page
    protected var toc: NodeSeq = NodeSeq.Empty
    // json descriptions of notes with length less than TipMaxSize
    protected val tips: scala.collection.mutable.ListBuffer[JValue] = ListBuffer()

    protected def processBody(ns: NodeSeq, params: procParams): NodeSeq = {

        def filterAttributes(node: Node) = node.attributes.filter(x => x.key == "lang" || x.key == "id")

        /*
        * Генерируемые css-классы:
        *
        * div.book
        * fb2-тег body. Базовый класс для книги; явно указан только в корневом div пая.
        *
        * div.book-section
        * fb2-теги section и annotation. Искусственный класс, не требующий определения стиля (мне он нужен для разметки
        * секций при делении книги на паи заданного размера).
        *
        * div.book-titleN(N=1..6)
        *
        * fb2-теги title и subtitle. Заголовки 1-6 уровня.
        *
        * span.book-code
        * fb2-тег code. Программные коды; обычно оформляется моноширинным шрифтом.
        *
        * div.book-epigraph
        * fb2-тег epigraph. Эпиграф книги или секции; обычно оформляется курсивом и отбивкой блока к правому краю с
        * выравниванием текста по левому краю блока. Может содержать внутри форматированный текст,
        * div class="book-author", div class="book-poem", div class="book-cite".
        *
        * div.book-cite
        * fb2-тег cite. Цитата внутри текста книги; оформляется по-разному (курсивным блоком несколько уже шириной, чем
        * окружающий текст; выделением с рамкой; отступом справва для всего блока и т.д.). Может содержать форматированный
        * текст, div class="book-author", div class="book-poem", table.
        *
        * div.book-author
        * fb2-тег text-author. Автор фрагмента текста, встречается только внутри div class="book-poem",
        * div class="book-cite", div class="book-epigraph".
        *
        * div.book-epigraph.book-cite
        * Цитата внутри эпиграфа; обычно оформляется как просто эпиграф.
        *
        * div.book-epigraph.book-author
        * Автор эпиграфа; обычно формление отличается от самого эпиграфа только выравниванием - по правому краю блока.
        *
        * div.book-cite.book-author
        * Автор цитаты; обычно формление отличается от самой цитаты только выравниванием - по правому краю блока.
        *
        * div.book-poem
        * fb2-тег poem. Стихотворение; может содержать внутри заголовок, эпиграф, div class="book-poem-stanza". Обычно
        * оформляется выравниванием блока по центру страницы.
        *
        * div.book-poem-stanza
        * fb2-тег stanza. Стихотворная строфа (четверостишье); может содержать внутри заголовки и
        * span class="book-poem-v". Обычно оформляется отступом сверху и снизу от окружающего текста.
        *
        * span.book-poem-v
        * fb2-тег v. Строка стихотворения; содержит внутри форматированный текст.
        *
        * div.book-poem.book-author
        * Автор стихотворения; обычно оформление отличается от самого стихотворения только выравниванием - по правому
        * краю блока.
        */
        def transformChain: NodeSeq => NodeSeq = {
            "body" #> ((tagSeq: NodeSeq) => {

                val bodyName = if (tagSeq.head.attribute("name").isEmpty) "" else tagSeq.head.attribute("name").head.text
                val idAttr = if ((tagSeq \ "title").isEmpty) {
                    val tocHeader = if (bodyName == "notes") "Примечания" else "В начало"
                    linkCounter += 1
                    val id = linkPrefix + linkCounter
                    toc = toc ++ <a href="#" goto-contents={id} class={"toc-level" + params.sectionLevel}>{tocHeader}</a> ++ <br/>
                    Attribute(null, "id", id, Null)
                } else Null

                <div class={DefaultCssPrefix}>{processBody(tagSeq.head.child, params(bodyName = bodyName))}</div> % idAttr

            }) &
                "section" #> ((tagSeq: NodeSeq) => {
                    val id = if (tagSeq.head.attribute("id").isEmpty) {linkCounter += 1; linkPrefix + linkCounter} else tagSeq.head.attribute("id").get.head.text

                    val result = <div id={id} class={DefaultCssPrefix + "-section"}>{
                        processBody(tagSeq.head.child,
                            params(sectionLevel = params.sectionLevel + 1, headerCount = 0)
                        )
                        }</div> % tagSeq.head.attributes.filter(x => x.key == "lang")

                    if (params.bodyName == "notes") {
                        if (result.text.length <= TipMaxSize && (result \\ "img").isEmpty)
                            tips += ("tip" -> id) ~ ("content" -> result.mkString)
                    }
                    result
                }) &
                "annotation" #> ((tagSeq: NodeSeq) => {
                    // этот тег будет обработан только внутри текста книги; аннотация к самой книге в processBody не передаётся
                    // (а передаются только все её дочерние элементы)
                    val id = if (tagSeq.head.attribute("id").nonEmpty) tagSeq.head.attribute("id").get.head.text
                    else {
                        linkCounter += 1; linkPrefix + linkCounter
                    }
                    toc = toc ++ <a href="#" goto-contents={id} class={"toc-level" + params.sectionLevel}>Аннотация</a> ++ <br/>;

                    <div id={id} class={DefaultCssPrefix + "-section"}>{processBody(tagSeq.head.child, params(sectionLevel = params.sectionLevel + 1))}</div> %
                        tagSeq.head.attributes.filter(_.key == "lang")
                }) &
                "title" #> ((tagSeq: NodeSeq) => {
                    val id = if (tagSeq.head.attribute("id").nonEmpty) tagSeq.head.attribute("id").get.head.text
                    else {
                        linkCounter += 1; linkPrefix + linkCounter
                    }
                    if (params.bodyName != "notes" || params.sectionLevel <= 1) {
                        val ptitle = (tagSeq \\ "p").map{_.text}.toList.join(". ")
                        val texttitle = if (tagSeq.text.length > 50) (tagSeq.text.substring(0, 47) + "...") else tagSeq.text
                        toc = toc ++ <a href ="#" goto-contents={id} class={"toc-level" + params.sectionLevel}>{if (ptitle.isEmpty) texttitle else ptitle}</a> ++ <br/> ;
                    }
                    <div id={id} class={DefaultCssPrefix + "-title" + params.sectionLevel}>{
                        processBody(tagSeq.head.child, params(headerCount = 0))
                        }</div>
                }) &
                "subtitle" #> ((tagSeq: NodeSeq) => {
                    val id = if (tagSeq.head.attribute("id").nonEmpty) tagSeq.head.attribute("id").get.head.text else {linkCounter += 1; linkPrefix + linkCounter}
                    if (params.bodyName != "notes" || params.sectionLevel <= 1) {
                        val ptitle = (tagSeq \\ "p").map{_.text}.toList.join(". ")
                        val texttitle = if (tagSeq.text.length > 50) (tagSeq.text.substring(0, 47) + "...") else tagSeq.text
                        toc = toc ++ <a href ="#" goto-contents={id} class={"toc-level" + params.sectionLevel + 1}>{if (ptitle.isEmpty) texttitle else ptitle}</a> ++ <br/> ;
                    }

                    <div id={id} class={DefaultCssPrefix + "-title" + (params.sectionLevel + 1)}>{
                        processBody(tagSeq.head.child, params(headerCount = 0))
                        }</div>
                }) &
                "emphasis" #> ((tagSeq: NodeSeq) => {
                    Text(" ") ++ <i>{processBody(tagSeq.head.child, params)}</i> ++ Text(" ")
                }) &
                "strong" #> ((tagSeq: NodeSeq) => {
                    <b>{processBody(tagSeq.head.child, params)}</b>
                }) &
                "strikethrough" #> ((tagSeq: NodeSeq) => {
                    <del>{processBody(tagSeq.head.child, params)}</del>
                }) &
                "image" #> ((imageTagSeq: NodeSeq) => {
                    val imageSrc = (imageTagSeq \ "@{http://www.w3.org/1999/xlink}href").text.tail
                    val (href, mw, h) = params.binariesMap.getOrElse(imageSrc, (imageSrc, 0, 0))
                    <img></img> % Attribute(null, "src", href, Null) % Attribute(null, "mw", mw.toString, Null) % Attribute(null, "h", h.toString, Null)
                }) &
                "code" #> ((tagSeq: NodeSeq) => {
                    <span class={DefaultCssPrefix + "-code"}>{processBody(tagSeq.head.child, params)}</span>
                }) &
                "empty-line" #> <br /> &
                "epigraph" #> ((tagSeq: NodeSeq) => {
                    <div class={DefaultCssPrefix + "-epigraph"}>{processBody(tagSeq.head.child, params(cssPrefix = "epigraph"))}</div>
                })  &
                "text-author" #> ((tagSeq: NodeSeq) => {
                    <div class={DefaultCssPrefix + "-author"}>{processBody(tagSeq.head.child, params)}</div> % filterAttributes(tagSeq.head)
                }) &
                "poem" #> ((tagSeq: NodeSeq) => {
                    <div class={DefaultCssPrefix + "-poem"}>{processBody(tagSeq.head.child, params(cssPrefix = "poem"))}</div> % filterAttributes(tagSeq.head)
                }) &
                "stanza" #> ((tagSeq: NodeSeq) => {
                    <div class={DefaultCssPrefix + "-poem-stanza"}>{processBody(tagSeq.head.child, params(sectionLevel = params.sectionLevel + 1))}</div> % filterAttributes(tagSeq.head)
                })  &
                "v" #> ((tagSeq: NodeSeq) => {
                    <span class={DefaultCssPrefix + "-poem-v"}>{processBody(tagSeq.head.child, params)}</span> % filterAttributes(tagSeq.head)
                }) &
                "date" #> ((tagSeq: NodeSeq) => {
                    <span>{tagSeq.head.text}</span> % filterAttributes(tagSeq.head)
                }) &
                "cite" #> ((tagSeq: NodeSeq) => {
                    <div class={DefaultCssPrefix + "-cite"}>{processBody(tagSeq.head.child, params)}</div> % filterAttributes(tagSeq.head)
                }) &
                "table" #> ((tagSeq: NodeSeq) => {
                    <table>{processBody(tagSeq.head.child, params)}</table> % filterAttributes(tagSeq.head)
                }) &
                "th" #> ((tagSeq: NodeSeq) => {
                    <th>{processBody(tagSeq.head.child, params)}</th> % filterAttributes(tagSeq.head)
                }) &
                "td" #> ((tagSeq: NodeSeq) => {
                    <td>{processBody(tagSeq.head.child, params)}</td> % filterAttributes(tagSeq.head)
                }) &
                "p" #> ((tagSeq: NodeSeq) => {
                    val id = tagSeq.head.attribute("id")
                    // kostyl detected:
                    // without manual creating of xml.Elem p-tags will contain all namespace bindings such increasing output html size
                    new xml.Elem(null, "p", if (id.isEmpty) Null else Attribute(None, "id", Text(id.head.text), Null), TopScope, true, processBody(tagSeq.head.child, params) :_*)
                }) &
                "a" #> ((tagSeq: NodeSeq) => {
                    val target = tagSeq.head.attribute("http://www.w3.org/1999/xlink", "href").head.text
                    if (target.startsWith("#")) {
                        linkCounter += 1
                        val myID = linkPrefix + linkCounter
                        <a href="#" goto={target.tail} id={myID}>{tagSeq.head.text}</a>
                    }
                    else
                        <a href={target} target="_blank">{tagSeq.head.text}</a>
                })
            // TODO: <style>
        }

        if (ns.isEmpty)
            ns
        else
            transformChain(ns)
    }

    protected def parseDescription(description: NodeSeq, binariesMap: Map[String, (String, Int, Double)]) = {
        val result = new BookDescription()
        if (description \ "title-info" nonEmpty) {
            result.title = (description \ "title-info" \ "book-title").text
            result.authors = (for (author <- description \ "title-info" \ "author") yield
                new AuthorName(author \ "first-name" text,
                    author \ "middle-name" text,
                    author \ "last-name" text,
                    author \ "nickname" text,
                    author \ "email" text)).toList
            result.genres = (for (genre <- description \ "title-info" \ "genre") yield genre.text).toList

            def transformToFlatText(ns: NodeSeq): NodeSeq = {
                val tr = "*" #> ((tagSeq: NodeSeq) => {
                    val tag = tagSeq.head
                    if (tag.child.length == 0)
                        tag
                    else if (List("strong", "emphasis", "style", "a", "strikethrough", "sub", "sup", "code").contains(tag.label))
                        Text(" " + transformToFlatText(tag.child).mkString + " ")
                    else
                        Text(transformToFlatText(tag.child).mkString + "\r\n")
                })
                tr(ns)
            }
            result.description = transformToFlatText(description \ "title-info" \ "annotation" \ "_").mkString

            val coverSrc =
                if ((description \ "title-info" \ "coverpage" nonEmpty)
                    && (description \ "title-info" \ "coverpage" \ "image" nonEmpty)
                    && (description \ "title-info" \ "coverpage" \ "image" \ "@{http://www.w3.org/1999/xlink}href").headOption.getOrElse(nse).text.nonEmpty)
                    (description \ "title-info" \ "coverpage" \ "image" \ "@{http://www.w3.org/1999/xlink}href").head.text.tail
                else
                    ""
            result.coverpage = binariesMap.getOrElse(coverSrc, (coverSrc, 0, 0))._1

            result.keywords = (for (keyword <- description \ "title-info" \ "keywords") yield keyword.text).toList
            result.date = if (description \ "title-info" \ "date" isEmpty) "" else (description \ "title-info" \ "date").head.attribute("value").getOrElse(description \ "title-info" \ "date" text).toString
            result.language = (description \ "title-info" \ "lang").text
            result.originalLanguage = (description \ "title-info" \ "src-lang").text
            result.translators = (for (translator <- description \ "title-info" \ "translator") yield
                new AuthorName(translator \ "first-name" text,
                    translator \ "middle-name" text,
                    translator \ "last-name" text,
                    translator \ "nickname" text,
                    translator \ "email" text)).toList
            result.sequence = (for (sequence <- description \ "title-info" \ "sequence") yield
                new Sequence(sequence.attribute("name").getOrElse(nse).text, sequence.attribute("number").getOrElse(nse).text)).toList
        }
        if (description \ "document-info" nonEmpty) {
            result.id = (description \ "document-info" \ "id").text
            result.fb2version = (description \ "document-info" \ "version").text
        }
        if (description \ "publish-info" nonEmpty) {
            result.publisher = (description \ "publish-info" \ "publisher").text
            result.publishTitle = (description \ "publish-info" \ "book-name").text
            result.publishCity = (description \ "publish-info" \ "city").text
            result.publishYear = (description \ "publish-info" \ "year"). text
        }
        result
    }

    protected def lengthOfBranch(input: NodeSeq) =
        input.mkString.length + (for (nestedTag <- input \\ "img") yield ImageSizeConstant).sum

    protected def divideHtmlTree(input: NodeSeq, currentSize: Int, currentSectionLevel: Int ): Seq[Node] = {
        (for (branch <- input)
        yield {
            if (lengthOfBranch(branch) < PieMaxSize)
            // если ветка (узел верхнего уровня во входной последовательности узлов) подходит по размеру,
            // то возвращаем её без изменений
                branch
            else {
                val sizes = for (child <- branch.child.zipWithIndex) yield {(child, lengthOfBranch(child._1))}

                if (sizes.length == 0)
                // вообще, весьма сомнительно, чтобы ветка не прошла по размеру и не имела дочерних узлов,
                // разве что это - один здоровый кусок текста, который мы делить не будем, а возьмём как есть
                    branch
                else {
                    // итерируем по дочерним узлам ветки, фиксируя левую границу окна и перемещая правую на каждом шаге,
                    // как только суммарный размер узлов в окне превзойдёт PieMaxSize, мы вернём новый узел со всеми
                    // атрибутами вернего узла ветки, но дочерними у него будут только узлы в окне

                    // это текущий размер окна
                    var cs = currentSize
                    // это левая граница окна
                    var lastUsedIndex = 0

                    val branchAttributes = (for (attr <- branch.attributes) yield if (attr.key == "class") Attribute(null, attr.key, attr.value.text + " split", Null) else attr).foldLeft[MetaData](Null)(_.append(_))

                    (for ((indexedChild, size) <- sizes
                          // в yield-выражение мы упадём, если размер окна превысил PieMaxSize,
                          // или мы дошли до конца коллекции, или наткнулись на начало новой главы
                          // (section самого верхнего уровня)
                          if (cs + size > PieMaxSize)
                              || (indexedChild._2 == sizes.length - 1)
                              || (currentSectionLevel < 2 && indexedChild._1.label == "div"
                              && indexedChild._1.attribute("class").getOrElse(NodeSeq.Empty).text == DefaultCssPrefix + "-section")
                              || {
                              cs = cs + size
                              false
                          })
                    yield {
                        // на этот момент у нас накоплено несколько узлов в окне, по сумме меньших PieMaxSize

                        // бьём текущий узел на куски так, чтобы первый кусок был равен PieMaxSize минус размер окна
                        val dividedChildren = divideHtmlTree(indexedChild._1, cs,
                            currentSectionLevel + (if (branch.attribute("class").getOrElse(NodeSeq.Empty).text == DefaultCssPrefix + "-section") 1 else 0))
                        val yieldResult =
                            if (dividedChildren.length > 0)
                            // если текущий узел разбили на куски, то первый узел результирующего разбиения будет
                            // содержать накопленные ранее узлы + первый узел из разбиения, последующие узлы
                            // в точности соответствуют остающемуся разбиению текущего узла
                                new Elem(branch.prefix, branch.label, if (lastUsedIndex == 0) branch.attributes else branchAttributes, branch.scope, false,
                                    (branch.child.slice(lastUsedIndex, indexedChild._2) ++ dividedChildren.head) :_*) ++
                                    (for (newBranch <- dividedChildren.tail) yield {
                                        val attrs = for (attr <- newBranch.attributes) yield if (attr.key == "class" && !attr.value.text.contains("split")) Attribute(null, attr.key, attr.value.text + " split", Null) else attr
                                        new Elem(branch.prefix, branch.label, branchAttributes, branch.scope, false,
                                            new Elem(newBranch.prefix, newBranch.label, attrs.foldLeft[MetaData](Null)((a,b)=> a.append(b)), newBranch.scope, false, newBranch.child:_*))
                                    })
                            else
                            // если текущий узел не разбился (был и так маленьким, либо состоял из одного-единственного тега),
                            // то в результирующем разбиении будут накопленные ранее узлы + текущий узел
                                new Elem(branch.prefix, branch.label, if (lastUsedIndex == 0) branch.attributes else branchAttributes, branch.scope, false,
                                    (branch.child.slice(lastUsedIndex, indexedChild._2) ++ indexedChild._1) :_*)

                        lastUsedIndex = indexedChild._2 + 1
                        cs = 0
                        yieldResult
                    }).flatten
                }
            }
        }).flatten
    }

    def convertToHtml(fullPath: String, outputFolder: String, pieMaxSize: Int, tipMaxSize: Int, srcPrefix: String, useExternalFixer: Boolean): Box[BookDescription] = {
        try {
            val input = scala.xml.XML.loadFile(fullPath)

            val validated = if (useExternalFixer)
                Fb2Fix(fullPath)
            else
                input.label == "FictionBook"

            if (validated) {

                val outputPath = Paths.get(outputFolder)

                if (Files.exists(outputPath))
                    for (oldFilename <- new File(outputFolder).listFiles()){
                        if (oldFilename.getName != new File(fullPath).getName) oldFilename.delete()
                    }
                else
                    Files.createDirectory(outputPath)

                // SAVE BINARIES
                val binariesMap =
                    (for ((binaryTag, index) <- (input \ "binary").zipWithIndex)
                    yield {
                        if ((binaryTag \ "@content-type").text.startsWith("image/")) {
                            val decodedData = new BASE64Decoder().decodeBuffer(binaryTag.text)
                            val imageStream = ImageIO.createImageInputStream(new ByteArrayInputStream(decodedData))

                            val imageAttrs: (String, Int, Double) = if (imageStream == null) {
                                info(fullPath + ": could not create ImageInputStream for " + (binaryTag \ "@id").text)
                                ((binaryTag \ "@content-type").text.substring("image/".length), 0, 0.0)
                            }
                            else {
                                val image = ImageIO.read(imageStream)
                                val it = ImageIO.getImageReaders(imageStream)
                                // trying to determine actual image format. if it fails - take mime type pointed in input fb2
                                (if (it.hasNext) it.next().getFormatName.toLowerCase else (binaryTag \ "@content-type").text.substring("image/".length), image.getWidth, image.getHeight * 1.0 / image.getWidth)
                            }

                            // save to filesystem
                            val binaryFileName = outputFolder + File.separator + index.toString + "." + imageAttrs._1
                            val binaryFile = new FileOutputStream(new File(binaryFileName))
                            val binaryHref = srcPrefix + index.toString + "." + imageAttrs._1

                            binaryFile.write(decodedData)
                            binaryFile.flush()
                            binaryFile.close()
                            (binaryTag \ "@id" text, (binaryHref, imageAttrs._2, imageAttrs._3))
                        }
                        else (binaryTag \ "@id" text, ("", 0 , 0.0))

                    }).toMap

                // CONVERT BODIES
                linkCounter = 0
                toc = NodeSeq.Empty
                PieMaxSize = if (pieMaxSize == 0) 10000 else pieMaxSize
                TipMaxSize = if (tipMaxSize == 0) 500 else tipMaxSize

                // need to trim input xml otherwise every linebreak is loaded as a separate tag labeled "#PCDATA"
                // with empty but non-zero length text which leads to output size counting errors
                val html = xml.XML.loadString(processBody(xml.Utility.trim(input) \ "body", new procParams(binariesMap, 1, 0, null))
                           .mkString.replace("  ", " ").replace(" .", ".").replace(" ,", ","))
                val htmlWriter = Files.newBufferedWriter(Paths.get(outputFolder + File.separator + "whole.html"), Charset.forName("UTF-8"))
                htmlWriter.write(html.mkString)
                htmlWriter.flush()
                htmlWriter.close()

                val htmlDivided = divideHtmlTree(html, 0, 0)
                // let's compute total html size including images and empty divs that can be added later
                val htmlSize = htmlDivided.foldLeft[Double](0.0)((sum: Double, branch: Node) => sum + lengthOfBranch(branch) + (if (branch.length == 1) <div></div>.mkString.length else 0))

                val tocWriter = Files.newBufferedWriter(Paths.get(outputFolder + File.separator + "contents.html"), Charset.forName("UTF-8"))
                tocWriter.write(toc.mkString)
                tocWriter.flush()
                tocWriter.close()

                var pieCounter = 0
                var pieSize = 0.0
                var pieMax = 0.0
                var pie = NodeSeq.Empty

                val description = JObject( List(
                    JField("pages", JArray(
                        (for (tag <- htmlDivided)
                        yield {
                            pie = tag
                            // add empty div if pie consists of the only root tag
                            if (pie.length == 1)
                                pie = pie ++ Text("\r\n") ++ <div></div>

                            val pieWriter = Files.newBufferedWriter(Paths.get(outputFolder + File.separator + pieCounter.toString + ".pie"), Charset.forName("UTF-8"))
                            pieWriter.write(pie.mkString)
                            pieWriter.flush()
                            pieWriter.close()

                            pieSize = lengthOfBranch(pie)
                            pieMax += pieSize

                            val yieldResult = ("number" -> pieCounter) ~
                                ("pie" -> pieSize / htmlSize) ~
                                ("pieMin" -> (pieMax - pieSize) / htmlSize) ~
                                ("pieMax" -> pieMax / htmlSize) ~
                                ("ids" -> (for (nestedTag <- pie \\ "_" if (nestedTag\"@id" nonEmpty)) yield nestedTag\"@id" text))

                            pie = NodeSeq.Empty
                            pieCounter += 1
                            yieldResult
                        }).toList
                    )),
                    JField("tips", JArray(tips.toList))
                ))

                val jsonWriter = Files.newBufferedWriter(Paths.get(outputFolder + File.separator + "description.json"), Charset.forName("UTF-8"))
                jsonWriter.write(json.Printer.pretty(render(description)))
                jsonWriter.flush()
                jsonWriter.close()

                Full(parseDescription(input \ "description", binariesMap))
            } else {
                info(s"$fullPath: is not valid fb2")
                Empty
            }
        } catch {
            case e: Exception =>
                error(e.getMessage)
                debug(e.getStackTraceString)
                Empty
        }
    }
}
