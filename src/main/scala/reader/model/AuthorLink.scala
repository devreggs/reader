package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class AuthorLink extends LongKeyedMapper[AuthorLink] with IdPK {

    def getSingleton = AuthorLink

    object author extends MappedLongForeignKey(this, Author)

    object book extends MappedLongForeignKey(this, Book)

}


object AuthorLink extends AuthorLink with LongKeyedMetaMapper[AuthorLink] {
    override def dbTableName = "authorlinks"
}