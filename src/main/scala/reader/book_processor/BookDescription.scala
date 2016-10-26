package reader.book_processor

/**
 * Created by IvanchukovaEV on 26.12.13.
 */
class BookDescription (var title: String,
                       var subtitles: List[String],
                       var authors: List[AuthorName],
                       var genres: List[String],
                       var description: String,
                       var coverpage: String,
                       var keywords: List[String],
                       var date: String,
                       var language: String,
                       var originalLanguage: String,
                       var translators: List[AuthorName],
                       var sequence: List[Sequence],
                       var edition: String,
                       var fullTitle: String,
                       var id: String,
                       var fb2version: String,
                       var publisher: String,
                       var publishTitle: String,
                       var publishCity: String,
                       var publishYear: String) {
    def this() = this ("", List(), List(), List(), "", "", List(), "", "", "", List(), List(), "", "", "" ,"", "", "", "", "")

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
}

class AuthorName (val firstName: String,
                     val middleName: String = "",
                     val lastName: String = "",
                     val nickname: String = "",
                     val email: String = "") {

    def mkString = {
        s"$firstName $middleName $lastName $nickname, e-mail: $email"
    }
}

class Sequence (val name: String,
                   val number: String) {
    def mkString = {
        s"$name - $number"
    }
}
