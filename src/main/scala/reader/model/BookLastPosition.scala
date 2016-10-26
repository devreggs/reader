package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList

class BookLastPosition extends LongKeyedMapper[BookLastPosition] with IdPK {

    def getSingleton = BookLastPosition

    object book extends MappedLongForeignKey(this, Book)

    object user extends MappedLongForeignKey(this, User)

    object position extends MappedDouble(this) {
        override def apply(value: Double) = {
            accessDateTime(new Date)
            super.apply(value)
        }
    }

    object accessDateTime extends MappedDateTime(this)

    override def saveMe = {
        if(user.foreign.isDefined)
            super.saveMe
        else
            this
    }
}

object BookLastPosition extends BookLastPosition with LongKeyedMetaMapper[BookLastPosition] {

    def get(user: User, book: Book) = {
        find(By(BookLastPosition.user, user.id.get),
                    By(BookLastPosition.book, book.id.get))
                .openOr(create.user(user.id.get).book(book.id.get).position(0.0).saveMe())
    }

    override def dbTableName = "booklastpositions"

    def removeFor(user: User) {
        bulkDelete_!!(By(BookLastPosition.user, user.id.get))
    }

    def removeFor(book: Book) {
        bulkDelete_!!(By(BookLastPosition.book, book.id.get))
    }
}