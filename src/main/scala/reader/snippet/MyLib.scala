package reader.snippet

import net.liftweb.util._
import net.liftweb.common._
import reader.model._
import Helpers._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JE.JsRaw
import reader.lib.SecurityGateway
import reader.lib.HelpSnippets._
import reader.lib.Environment
import java.util.Date

class MyLib extends Logger{

    def bookList = {
        val books = User.currentUser.books.refresh.filter(b => {
            (b.state.get == BookState.individual ||
                b.state.get == BookState.published) &&
                (if(S.param("genre").isDefined) {
                    b.genres.find(_.id == S.param("genre").openOr("-1").toIntOr(-1)).isDefined
                } else if(S.param("author").isDefined) {
                    b.authors.find(_.id == S.param("author").openOr("-1").toIntOr(-1)).isDefined
                } else
                    true)
        }).sortBy(book => {
            val date = BookLastPosition.get(User.currentUser, book).accessDateTime.get
            if(date == null)
                new Date(0)
            else
                date
        })(Ordering[Date].reverse)

        ".book-block *" #> books.map(book => {
            ".book-block-title *" #> (book.title.get + ".") &
                ".book-block-author *" #> (book.authorTitle.get + ".") &
                ".book-block-link [href]" #> ("/view?id=" + book.id.get.toString) &
                ".book-block-image [src]" #> ("/book/%s/thumb-tile.jpg".format(book.id.get))
        })
    }

    def demoBooks = {
        ".book-block *" #> Environment.demoBooks.filter(SecurityGateway.allowRead(_)).map(book => {
            ".book-block-title *" #> (book.title.get + ".") &
                ".book-block-author *" #> (book.authorTitle.get + ".") &
                ".book-block-link [href]" #> ("/paged-reader?id=" + book.id.get.toString) &
                ".book-block-image [src]" #> ("/book/%s/thumb-tile.jpg".format(book.id.get))
        })
    }

    def readBookTitle = {
        val book = Book.byId(S.param("id")
            .openOrThrowException("fail open id param"))
            .openOrThrowException("fail book id")
        ".book-title *" #> (book.authorTitle + " \"" + book.title.get + "\"")
    }

    def readBookParams = {
        val book = Book.byId(S.param("id")
            .openOrThrowException("fail open id param"))
            .openOrThrowException("fail book id")

        val user = User.currentUser
        val position = BookLastPosition.get(user, book).position.get
        val fontSize = user.fontSize.get
        val fontFamily = user.fontFamily.get
        val textAlign = user.textAlign.get
        val theme = user.theme.get
        val link = S.param("link").openOr("")

        val scriptString = "var bookId = %s;".format(S.param("id").openOrThrowException("fail open id param")) +
        """var lastPosition = %s;
          var fontFamily = '%s';
          var fontSize = '%s';
          var textAlign = '%s';
          var theme = '%s';
          var link = '%s';""".format(position, fontFamily, fontSize, textAlign, theme, link)


        "*" #> JsCmds.Script(JsCmds.Run(JsRaw(scriptString).toJsCmd))
    }

    def sessMessage = {
        val msg = User.currentSessionMessage.openOr("")
        ".alert" #> showIf(msg.nonEmpty) andThen
        ".msg *+" #> msg
    }

    def bookFilter = {
        val user = User.currentUser
        var filter = "Фильтр"
        val isFilter = S.param("author").isDefined || S.param("genre").isDefined

        if(S.param("author").isDefined) {
            val author = Author.byId(S.param("author").openOr("-1").toIntOr(-1))
            filter = (if(author.isDefined)
                author.openOrThrowException("empty author box").fullNameLF.get
            else
                "")
        }
        if(S.param("genre").isDefined) {
            val genre = Genre.byId(S.param("genre").openOr("-1").toIntOr(-1))
            filter = (if(genre.isDefined)
                genre.openOrThrowException("empty genre box").title.get
            else
                "")
        }

        "#clear-filter-dialog" #> showIf(isFilter) &
        ".current-filter *" #> filter &
            ".genre *" #> user.myGenres.sortBy(_.title.get).map(genre => {
                "a [href]" #> ("my-page?genre=" + genre.id.get.toString) &
                ".genre-name *" #> genre.title.get
            }) &
            ".author *" #> user.myAuthors.sortBy(_.shortNameLF.get).map(author => {
                "a [href]" #> ("my-page?author=" + author.id.get.toString) &
                    ".author-name *" #> author.fullNameLF.get
            })
    }

    def bookView = {
        val bookBox = Book.byId(S.param("id").openOr("-1"))

        if(bookBox.isDefined && SecurityGateway.allowRead(bookBox.get)) {

            val book = bookBox.get

            val position = "%2.2f" format (BookLastPosition.get(User.currentUser, book).position.get) * 10000.0 / 100.0

            "#book-view-remove-item" #> showIf(SecurityGateway.allowRemove(book)) &
                "#admin-block" #> showIf(SecurityGateway.allowAdmin) andThen
            ".book-view-title *" #> book.title.get &
                ".book-view-author *" #> book.authorTitle.get &
                ".book-view-description *" #> book.description.get &
                ".book-view-genre *" #> book.genres.map(_.title.get).mkString(", ") &
                ".book-view-position *" #> position &
                ".book-view-image [src]" #> ("/book/%s/thumb-view.jpg".format(book.id.get)) &
                ".book-view-read-link [href]" #> ("/paged-reader?id=%s" format book.id.get.toString) &
                ".book-view-remove-link [href]" #> ("/remove/book/%s" format book.id.get.toString) &
                ".book-view-contents" #> PCDataXmlParser("<div>" + book.readFile("parsed/contents.html") + "</div>") &
                "#book-view-originalFilename *" #> book.originalFileName.get &
                "#book-view-download [href]" #> "download-original/%d".format(book.id.get) &
                "#toggle-state" #> SHtml.ajaxCheckbox(book.state.get == BookState.published, (value: Boolean) => {
                        if(value)
                            book.state(BookState.published).saveMe()
                        else
                            book.state(BookState.individual).saveMe()
                        JsCmds.Noop
                    })

        } else
            "*" #> ""
    }
}
