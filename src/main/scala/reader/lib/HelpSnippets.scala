package reader.lib

import net.liftweb.util.ClearNodes
import net.liftweb.http.{ForbiddenResponse, LiftResponse, Req}
import net.liftweb.common.Full
import net.liftweb.common.Logger

object HelpSnippets extends Logger {

    implicit class CMSString(val str: String) {

        def digest(length: Int) = {
            if(str.length > length)
                str.substring(0, length - 4) + "..."
            else
                str
        }

        def nameHead = {
            str.split(' ').map(part => {
                if(part.size > 2) {
                    part.substring(0, 1).toLowerCase() match {
                        case "дж" => "Дж"
                        case _ => part.head.toString.capitalize
                    }
                } else if(part.size == 1){
                    part.head.toString.capitalize
                }
            }).mkString(". ") + "."
        }

        def nameNormalize = {
            str.split(' ').map(_.toLowerCase.capitalize).mkString(" ")
        }

        def nameCapitalize = {
            str.toLowerCase.capitalize
        }

        def toIntOr(value: Int) = {
            var result = value
            try {
                result = str.toInt
            } catch {
                case _: Throwable => true
            }
            result
        }

        def toLongOr(value: Long) = {
            var result = value
            try {
                result = str.toLong
            } catch {
                case _: Throwable => true
            }
            result
        }

        def toDoubleOr(value: Double) = {
            var result = value
            try {
                result = str.toDouble
            } catch {
                case _: Throwable => true
            }
            result
        }
    }

    def showIf(condition: Boolean) = {
        if(condition)
            net.liftweb.util.PassThru
        else
            ClearNodes
    }

    def stdResponse(req: Req)(body: => LiftResponse) = {
        try {
            info("Serve " + req.path.wholePath.mkString("/"))
            val result = body
            info(req.path.wholePath.mkString("/") + " complete")
            Full(result)
        } catch {
            case e:Exception => {
                error(req.path.wholePath.mkString("/") + " " + e.getMessage)
                Full(ForbiddenResponse())
            }
        }
    }

}
