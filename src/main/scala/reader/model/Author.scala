package reader.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.mapper.ByList
import net.liftweb.mapper.ManyToMany
import reader.lib.HelpSnippets._
import net.liftweb.common.Empty

class Author extends LongKeyedMapper[Author] with IdPK with ManyToMany {

    def getSingleton = Author

    object fullNameFL extends MappedString(this, 100)

    object fullNameLF extends MappedString(this, 100)

    object shortNameFL extends MappedString(this, 100)

    object shortNameLF extends MappedString(this, 100)

    object email extends MappedString(this, 100)

    object nickname extends MappedString(this, 50)

    object books extends MappedManyToMany(AuthorLink, AuthorLink.author, AuthorLink.book, Book)


}

object Author extends Author with LongKeyedMetaMapper[Author] {
    override def dbTableName = "authors"

    def byId(id: Long) = {
        find(By(Author.id, id))
    }

    def byName(name: String) = {
        find(By(Author.fullNameFL, name)) or
            find(By(Author.fullNameLF, name)) or
            find(By(Author.shortNameFL, name)) or
            find(By(Author.shortNameLF, name)) or
            Empty
    }
}