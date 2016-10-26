package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class Genre extends LongKeyedMapper[Genre] with IdPK with ManyToMany {

    def getSingleton = Genre

    object sourceTitle extends MappedString(this, 50)

    object title extends MappedString(this, 50)

    object parent extends MappedLongForeignKey(this, Genre)

    object books extends MappedManyToMany(GenreLink, GenreLink.genre, GenreLink.book, Book)

}

object Genre extends Genre with LongKeyedMetaMapper[Genre] {
    override def dbTableName = "genres"

    def byId(id: Long) = {
        find(By(Genre.id, id))
    }

    def bySourceTitle(title: String) = {
        find(By(Genre.sourceTitle, title))
    }
}