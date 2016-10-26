package reader.snippet

import net.liftweb.util._
import Helpers._
import net.liftweb.http.{S, RequestVar, SHtml}
import java.net.{HttpURLConnection, URL}
import reader.model.{BookLastPosition, User, Book}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JE.JsRaw

class Upload {
    private object urlString extends RequestVar("")

    def urlUploadFormSubmit() = {
        val url = new URL(urlString)
        val connection  = url.openConnection().asInstanceOf[HttpURLConnection]
        val responseCode = connection.getResponseCode()
        if (responseCode == HttpURLConnection.HTTP_OK) {

        }
    }

    def urlUploadForm = {
        "name=input-url" #> SHtml.text(urlString, x => urlString(x)) &
            "type=submit" #> SHtml.onSubmitUnit(() => {
                urlUploadFormSubmit()
            })
    }

    def uploadParams = {
        val user = User.currentUser
        val scriptString = "var freeStorageSize = %s;".format(user.freeSpace)
        "*" #> JsCmds.Script(JsCmds.Run(JsRaw(scriptString).toJsCmd))
    }
}
