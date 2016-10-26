package reader.comet

import net.liftweb.http.{CometListener, SHtml, CometActor}
import reader.model.{User, BookState, Book}
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.{JsVar, JsFunc, JsRaw}
import net.liftweb.http.js.JsCmds.{Function, Script, Alert}
import net.liftweb.http.js.{JsCmds, JsExp}
import reader.lib.SecurityGateway

class Processing extends CometActor with CometListener {

    def registerWith = ProcessingLobby

    this ! UpdateUserBooks(User.currentUser.id.is)

    def render = {
            val user = User.currentUser
            ".book-block *" #> user.rawAndFailed.map(book => {
                ".book-block-title *" #> book.originalFileName.get &
                    ".book-block-error-msg *" #> book.failedMessage.get &
                    (if (book.state.get == BookState.wrong)
                        ".load-image *" #> ""
                    else
                        ".remove-image *" #> "") andThen
                        ".remove-book [onclick]" #> SHtml.ajaxInvoke(() => {
                            book.delete_!
                            reRender()
                            JsCmds.Noop
                        })
            })
    }

    override def lowPriority = {
        case msg: UpdateUserBooks => {
            if(msg.userId == User.currentUser.id.is) reRender()
        }
    }

}