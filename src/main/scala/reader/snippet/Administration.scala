package reader.snippet

import net.liftweb.util._
import net.liftweb.common._
import reader.model._
import Helpers._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JE.JsRaw
import reader.lib.{BookProcessor, SecurityGateway, Environment}
import reader.lib.HelpSnippets._
import net.liftweb.mapper.StartAt

class Administration extends Logger {

    def environment = {
        val descimalFormat = new java.text.DecimalFormat("###,###,###,###,###.##")
        "#default-path" #> SHtml.ajaxText(Environment.defaultPath.value.get, (text: String) => {
            Environment.defaultPath.value(text).saveMe()
            JsCmds.Noop
        }) &
        "#demo-book-id-1" #> SHtml.ajaxSelect(("-1", "Нет") :: Book.published.map(book =>
                (book.id.get.toString, book.authorTitle + ". " + book.title)),
            Full(Environment.demoBookId1.value.get),
            (value: String) => {
                Environment.demoBookId1.value(value).saveMe()
                JsCmds.Noop
            }) &
            "#demo-book-id-2" #> SHtml.ajaxSelect(("-1", "Нет") :: Book.published.map(book =>
                (book.id.get.toString, book.authorTitle + ". " + book.title)),
                Full(Environment.demoBookId2.value.get),
                (value: String) => {
                    Environment.demoBookId2.value(value).saveMe()
                    JsCmds.Noop
                }) &
            "#demo-book-id-3" #> SHtml.ajaxSelect(("-1", "Нет") :: Book.published.map(book =>
                (book.id.get.toString, book.authorTitle + ". " + book.title)),
                Full(Environment.demoBookId3.value.get),
                (value: String) => {
                    Environment.demoBookId3.value(value).saveMe()
                    JsCmds.Noop
                }) &
            ".user-element *" #> User.findAll().map(user => {
                ".user-email *" #> user.email.get &
                    ".user-creation-date *" #> user.created.toString() &
                    ".user-role *" #> user.role.get.toString &
                    ".user-last-connection *" #> user.lastConnection.toString()
            }) &
            ".book-element *" #> Book.findAll().map(book => {
                ".book-title *" #> (book.authorTitle.get + ". " + book.title.get + ". " + book.originalFileName.get) &
                    ".book-creation-date *" #> book.obtainDate.toString() &
                    ".book-owner *" #> book.owner.foreign.openOrThrowException("empty owner").email.get &
                    ".book-state *" #> book.state.get.toString &
                    ".book-view [href]" #> ("/view?id=" + book.id.get.toString)
            }) &
            ".genre-element *" #> Genre.findAll().map(genre => {
                ".genre-source *" #> genre.sourceTitle.get &
                    ".genre-title *" #> SHtml.ajaxText(genre.title.get, str => {
                        genre.title(str).saveMe()
                        JsCmds.Noop
                    })
            }) &
            "#reprocess-all [onclick]" #> SHtml.ajaxInvoke(() => {
                BookProcessor.reprocessAll
                JsCmds.Noop
            }) &
            "#remove-all-failed [onclick]" #> SHtml.ajaxInvoke(() => {
                BookProcessor.removeAllWrongs
                JsCmds.Noop
            }) &
            "#drop-author-titles [onclick]" #> SHtml.ajaxInvoke(() => {
                BookProcessor.recalcAuthorTitles
                JsCmds.Noop
            }) &
            ".stat-values *" #> Map("Занято хранилища" -> Environment.storageSize).map(c => {
                ".stat-key *" #> c._1 &
                ".stat-value *" #> c._2.toString
            })
    }

}
