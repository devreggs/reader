package reader.lib

import net.liftweb.actor.LiftActor
import net.liftweb.http.SessionWatcherInfo


object SessionMonitor extends LiftActor {
    private var sessionSize = 0
    protected def messageHandler = {
        case SessionWatcherInfo(sessions) => {
            sessionSize = sessions.size
        }
    }
    def count = sessionSize
}
