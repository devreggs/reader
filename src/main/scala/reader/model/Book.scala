package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList
import reader.lib.HelpSnippets._
import reader.comet.{UpdateUserBooks, ProcessingLobby, Processing}

object BookState extends Enumeration {
    val raw, individual, wrong, published = Value
}

class Book extends LongKeyedMapper[Book]
            with FilesMapper
            with IdPK
            with ManyToMany {

    def getSingleton = Book

    object title extends MappedString(this, 150)

    object authorTitle  extends MappedString(this, 100)

    object authors extends MappedManyToMany(AuthorLink, AuthorLink.book, AuthorLink.author, Author)

    object genres extends MappedManyToMany(GenreLink, GenreLink.book, GenreLink.genre, Genre)

    object users extends MappedManyToMany(BookLink, BookLink.book, BookLink.user, User)

    object description extends MappedString(this, 2048)

    object coverPageFileName extends MappedString(this, 255)

    object state extends MappedEnum(this, BookState) {
        override def defaultValue = BookState.raw
    }

    object failedMessage extends MappedString(this, 1024)

    object originalFileName extends MappedString(this, 255)

    object fromURL extends MappedBoolean(this) {
        override def defaultValue = false
    }

    object owner extends MappedLongForeignKey(this, User)

    object obtainDate extends MappedDateTime(this) {
        override def defaultValue = new Date
    }

    override def saveMe() = {
        super.saveMe()
    }

    override def save = {
        super.save
    }

    override def delete_! = {
        BookLastPosition.removeFor(this)
        authors.clear()
        genres.clear()
        users.clear()
        save
        removeFiles
        val result = super.delete_!
        result
    }
}


object Book extends Book with LongKeyedMetaMapper[Book] {
    override def dbTableName = "books"

    def raw = {
        Book.findAll(By(Book.state, BookState.raw))
    }

    def wrong = Book.findAll(By(Book.state, BookState.wrong))

    def byId(id: String) = {
        Book.find(By(Book.id, id.toIntOr(-1)))
    }

    def byId(id: Int) = {
        Book.find(By(Book.id, id))
    }

    def individualAndPublished = {
        Book.findAll(By(Book.owner, User.currentUser), By(Book.state, BookState.individual)) ++
            Book.findAll(By(Book.state, BookState.published))
    }

    def published = Book.findAll(By(Book.state, BookState.published))
}