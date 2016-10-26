package reader.comet

import net.liftweb.http.{ListenerManager, SHtml, CometActor}
import reader.model.{User, BookState, Book}
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.{JsVar, JsFunc, JsRaw}
import net.liftweb.http.js.JsCmds.{Function, Script, Alert}
import net.liftweb.http.js.JsExp
import reader.lib.SecurityGateway
import net.liftweb.actor.LiftActor

case class UpdateUserBooks(userId: Long)

object ProcessingLobby extends LiftActor with ListenerManager {

    def createUpdate = UpdateUserBooks(-1)

    override def lowPriority = {
            case msg: UpdateUserBooks => {
                updateListeners(msg)
            }
    }

}
