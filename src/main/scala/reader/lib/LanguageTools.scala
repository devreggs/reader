package reader.lib

import org.apache.http.client.methods.RequestBuilder
import org.apache.http.util.EntityUtils
import org.apache.http.impl.client.DefaultHttpClient
import net.liftweb.http.rest
import rest._
import net.liftweb.json._
import JsonDSL._

object LanguageTools {

    val httpClient = new DefaultHttpClient()

    implicit val formats = DefaultFormats

    def translateText(text: String) = {

        case class YaTranslationExtractor(text: List[String])
        val yaAPI = "trnsl.1.1.20140709T114453Z.111085df9dae0598.909284dde5dd4651deec41328f048dbfd47be855"

        val httpGet = RequestBuilder.get()
            .setUri("https://translate.yandex.net/api/v1.5/tr.json/translate")
            .addParameter("key", yaAPI)
            .addParameter("text", text)
            .addParameter("lang", "ru")
            .addParameter("format", "plain")
            .addParameter("options", "1")
            .addParameter("callback", "")
            .build()

        val response = httpClient.execute(httpGet)

        val entity = response.getEntity()

        (if (entity != null) {
            (net.liftweb.json.parse(EntityUtils.toString(entity)).extract[YaTranslationExtractor].text.mkString(""))
        } else {
            ""
        })
    }

    def translateWord(word: String): List[String] = {

        case class YaTextExtractor(text: String)
        case class YaExampleExtractor(text: String, tr: List[YaTextExtractor])
        case class WordTranslationYaExtractor(text: String,
                                                      pos: String,
                                                      syn: List[YaTextExtractor],
                                                      mean: List[YaTextExtractor],
                                                      ex: List[YaExampleExtractor])
        case class WordTranslationArticleYaExtractor(text: String,
                                                     pos: String,
                                                     ts: String,
                                                     tr: List[WordTranslationYaExtractor])

        val yaAPI = "dict.1.1.20140710T042106Z.e26383b3b3fa38e3.b4df445ceadf9d00aced0a33d3256e7e534a3115"

        val httpGet = RequestBuilder.get()
            .setUri("https://dictionary.yandex.net/api/v1/dicservice.json/lookup")
            .addParameter("key", yaAPI)
            .addParameter("lang", "en-ru")
            .addParameter("text", word.trim())
            .addParameter("ui", "ru")
            .addParameter("flags", "12")
            .build()

        val response = httpClient.execute(httpGet)
        val entity = response.getEntity()

        (if (entity != null) {
            val str = EntityUtils.toString(entity)
            (net.liftweb.json.parse(str) \ "def").children.map(v => {
                val article = v.extract[WordTranslationArticleYaExtractor]
                "%s, %s: %s". format(article.text, article.pos, article.tr.map(_.text).mkString(", "))
            }).toList
        } else {
            List()
        })
    }
}
