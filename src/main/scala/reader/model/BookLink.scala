package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class BookLink extends LongKeyedMapper[BookLink] with IdPK {

    def getSingleton = BookLink

    object book extends MappedLongForeignKey(this, Book)

    object user extends MappedLongForeignKey(this, User)

}


object BookLink extends BookLink with LongKeyedMetaMapper[BookLink] {
    override def dbTableName = "booklinks"
}