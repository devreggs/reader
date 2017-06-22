package reader.book_processor

/**
 * Created by IvanchukovaEV on 26.12.13.
 */
class BookDescription (var title: String = "",
                       var subtitles: List[String] = List(),
                       var authors: List[AuthorName] = List(),
                       var genres: List[String] = List(),
                       var description: String = "",
                       var coverpage: String = "",
                       var keywords: List[String] = List(),
                       var date: String = "",
                       var language: String = "",
                       var originalLanguage: String = "",
                       var translators: List[AuthorName] = List(),
                       var sequence: List[Sequence] = List(),
                       var edition: String = "",
                       var fullTitle: String = "",
                       var id: String = "",
                       var fb2version: String = "",
                       var publisher: String = "",
                       var publishTitle: String = "",
                       var publishCity: String = "",
                       var publishYear: String = "") {

    def mkString = {
        s"Title: $title\r\n${subtitles.mkString("\r\n")}" +
            s"Authors (${authors.length}): ${(for (author <- this.authors) yield s"${author.mkString}").mkString("; ")}\r\n" +
            s"Language: $language\r\n" +
            s"Date: $date\r\n" +
            s"Genres (${genres.length}): ${this.genres.mkString(", ")}\r\n" +
            s"Description: $description\r\n" +
            s"Coverpage: $coverpage\r\n" +
            s"Sequence: ${this.sequence.map( (s: Sequence) => s.mkString).mkString(", ")}\r\n" +
            s"Original language: $originalLanguage\r\n" +
            s"Translators (${translators.length}): ${this.translators.map((t: AuthorName) => t.mkString).mkString("; ")}\r\n" +
            s"Publisher: $publisher\r\n" +
            s"Publish title: $publishTitle\r\n" +
            s"Publish city: $publishCity\r\n" +
            s"Publish year: $publishYear\r\n" +
            s"Keywords (${keywords.length}): ${keywords.mkString(", ")}\r\n" +
            s"ID: $id\r\n" +
            s"Version: $fb2version\r\n"
    }

    override def toString = mkString

    override def equals(other: Any) = {
        if (other.getClass != this.getClass)
            false
        else {
            val o = other.asInstanceOf[this.type]
            title == o.title &&
            authors == o.authors &&
            language == o.language &&
            date == o.date &&
            genres == o.genres &&
            description == o.description &&
            coverpage == o.coverpage &&
            sequence == o.sequence &&
            originalLanguage == o.originalLanguage &&
            translators == o.translators &&
            publisher == o.publisher &&
            publishTitle == o.publishTitle &&
            publishCity == o.publishCity &&
            publishYear == o.publishYear &&
            keywords == o.keywords &&
            id == o.id &&
            fb2version == o.fb2version
        }
    }
}

class AuthorName (val firstName: String,
                     val middleName: String = "",
                     val lastName: String = "",
                     val nickname: String = "",
                     val email: String = "") {

    def mkString = {
        s"fn:$firstName mn:$middleName ln:$lastName '$nickname', e-mail: $email"
    }

    override def toString = mkString

    override def equals(other: Any) = {
        if (other.getClass != this.getClass)
            false
        else {
            val o = other.asInstanceOf[this.type]
            firstName == o.firstName && middleName == o.middleName && lastName == o.lastName &&
            nickname == o.nickname && email == o.email
        }
    }
}

class Sequence (val name: String,
                   val number: String) {
    def mkString = {
        s"$name - $number"
    }
}
