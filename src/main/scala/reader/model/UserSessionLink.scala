package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class UserSessionLink extends LongKeyedMapper[UserSessionLink] with IdPK {

    def getSingleton = UserSessionLink

    object user extends MappedLongForeignKey(this, User)

}


object UserSessionLink extends UserSessionLink with LongKeyedMetaMapper[UserSessionLink] {
    override def dbTableName = "usersessionlinks"

    def byId(id: Long) = {
        find(By(UserSessionLink.id, id))
    }
}