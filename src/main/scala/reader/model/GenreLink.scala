package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class GenreLink extends LongKeyedMapper[GenreLink] with IdPK {

    def getSingleton = GenreLink

    object genre extends MappedLongForeignKey(this, Genre)

    object book extends MappedLongForeignKey(this, Book)

}


object GenreLink extends GenreLink with LongKeyedMetaMapper[GenreLink] {
    override def dbTableName = "genrelinks"
}