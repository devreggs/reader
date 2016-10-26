package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import js.jquery.JQueryArtifacts
import sitemap._
import net.liftweb.sitemap.Loc._
import mapper._

import reader.model._
import net.liftmodules.JQueryModule
import _root_.java.sql.{Connection, DriverManager}
import java.text.SimpleDateFormat
import reader.lib._
import net.liftweb.http.Html5Properties
import net.liftweb.common.Full
import java.util.prefs.Preferences
import reader.comet.Processing
import net.liftweb.http.Html5Properties
import net.liftweb.common.Full
import net.liftweb.sitemap.Loc.EarlyResponse
import net.liftweb.sitemap.Loc.TestAccess
import net.liftweb.http.provider.HTTPCookie
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory
import java.net.URL

/**
 * Менеджер для postgesql
 */
object DBVendor extends ConnectionManager with Logger {

    Class.forName("org.postgresql.Driver")

    def newConnection(name: ConnectionIdentifier) = {

        try {
            var dbHost = System.getenv("OPENSHIFT_POSTGRESQL_DB_HOST")
            var dbPort = System.getenv("OPENSHIFT_POSTGRESQL_DB_PORT")
            var dbUser = System.getenv("OPENSHIFT_POSTGRESQL_DB_USERNAME")
            var dbPassword = System.getenv("OPENSHIFT_POSTGRESQL_DB_PASSWORD")
			var dbName = System.getenv("OPENSHIFT_APP_NAME")

            if(dbHost == null && dbPort == null && dbUser == null && dbPassword == null) {
                dbHost = "localhost"
                dbPort = "5432"
                dbUser = "postgres"
                dbPassword = "postgres"
				dbName = "jetreader"
            }

            Full(DriverManager.getConnection( "jdbc:postgresql://" + dbHost + ":" + dbPort + "/" + dbName,
                dbUser, dbPassword))
        } catch {
            case e: Exception => {
                e.printStackTrace
                error("fail to create connection for db")
                System.exit(-1)
                Empty
            }
        }
    }

    def releaseConnection(conn: Connection) {
        conn.close
    }
}

/**
 * Форматирование дат и времени
 */
object RusDateTimeConverter extends DateTimeConverter with Loggable {
    val dateTimeFormatter = new SimpleDateFormat("dd.MM.yyyy hh:mm")
    val dateFormatter = new SimpleDateFormat("dd.MM.yyyy")
    val timeFormatter = new SimpleDateFormat("hh:mm")

    def formatDateTime(d : java.util.Date) = dateTimeFormatter.format(d)
    def formatDate(d : java.util.Date) = dateFormatter.format(d)
    def formatTime(d : java.util.Date) = timeFormatter.format(d)

    def parseDateTime(s : scala.Predef.String) = try {
        Full(dateTimeFormatter.parse(s))
    } catch {
        case x: Exception => {
            logger.error(x.getMessage())
            Empty
        }
    }

    def parseDate(s : scala.Predef.String) = try {
        Full(dateFormatter.parse(s))
    } catch {
        case x: Exception => {
            logger.error(x.getMessage())
            Empty
        }
    }

    def parseTime(s : scala.Predef.String) = try {
        Full(dateFormatter.parse(s))
    } catch {
        case x: Exception => {
            logger.error(x.getMessage())
            Empty
        }
    }
}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */

class Boot {
    def boot {

        val logDir = (if(System.getenv("OPENSHIFT_DATA_DIR") != null)
            System.getenv("OPENSHIFT_DATA_DIR")
        else
            ".")
        val logUrl = LiftRules.getResource("/default.logback.xml")
        logUrl.foreach(x => {
            Logger.setup = Full(() => {
                val context = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
                val jc = new JoranConfigurator()
                jc.setContext(context)
                context.reset()
                context.putProperty("LOG_PATH", logDir)
                jc.doConfigure(x)
            })
        })

        // Устанавливаем postgre
        DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

        Schemifier.schemify(true, Schemifier.infoF _,
            User,
            Book,
            Author,
            Genre,
            AuthorLink,
            GenreLink,
            EnvironmentValue,
            BookLastPosition,
            BookLink,
            UserSessionLink)

        // Где лежат сниппеты
        LiftRules.addToPackages("reader")

        // Карта сайта
        def sitemap = SiteMap(Menu.i("Стартовая страницы") / "index" >>
                TestAccess(() => if (!User.loggedIn_?) Empty else Full(RedirectResponse("/my-page", S.responseCookies: _*))) >> Hidden,

            Menu.i("Регистрация") / "create-account" >>
                TestAccess(() => if (!User.loggedIn_?) Empty else Full(RedirectResponse("/my-page", S.responseCookies: _*))) >> Hidden,

            Menu.i("Восстановление пароля") / "forgot-password" >>
                TestAccess(() => if (!User.loggedIn_?) Empty else Full(RedirectResponse("/my-page", S.responseCookies: _*))) >> Hidden,

            Menu.i("Полка") / "my-page" >>
                TestAccess(() => if (SecurityGateway.allowCabinet) Empty else Full(RedirectResponse("/index"))),

            /*Menu.i("В обработке") / "processing" >>
                TestAccess(() => if (SecurityGateway.allowUpload) Empty else Full(RedirectResponse("/index"))),*/

            Menu.i("Книга") / "view" >>
                TestAccess(() => if (SecurityGateway.allowCabinet) Empty else Full(RedirectResponse("/index"))) >>
                EarlyResponse(() => {
                if(Book.byId(S.param("id").openOr("-1")).isDefined &&
                    SecurityGateway.allowRead(Book.byId(S.param("id").openOr("-1")).get))
                    Empty
                else
                    Full(RedirectResponse("/index"))
            }) >> Hidden,

            Menu.i("Модуль чтения") / "paged-reader" >>
                EarlyResponse(() => {
                    if(Book.byId(S.param("id").openOr("-1")).isDefined &&
                        SecurityGateway.allowRead(Book.byId(S.param("id").openOr("-1")).get))
                        Empty
                    else
                        Full(RedirectResponse("/index"))
                }) >> Hidden,

            Menu.i("Выход") / "logout" >>
                TestAccess(() => if (User.loggedIn_?) Empty else Full(RedirectResponse("/index"))) >>
                EarlyResponse(() => {
                    User.logOut
                    Full(RedirectResponse("/index", S.responseCookies: _*))
                }) >> Hidden,

            Menu.i("Загрузка книг") / "upload" >>
                TestAccess(() => if (SecurityGateway.allowUpload) Empty else Full(RedirectResponse("/index"))),

            Menu.i("Администрирование") / "administration" >>
                TestAccess(() => if (SecurityGateway.allowAdmin) Empty else Full(RedirectResponse("/index")))
        )

        // set the sitemap.  Note if you don't want access control for
        // each page, just comment this line out.
        LiftRules.setSiteMapFunc(() => sitemap)

        //Init the jQuery module, see http://liftweb.net/jquery for more information.
        LiftRules.jsArtifacts = JQueryArtifacts
        JQueryModule.InitParam.JQuery = JQueryModule.JQuery182
        JQueryModule.init()

        LiftRules.maxMimeFileSize = Environment.maxFileSize

        LiftRules.maxMimeSize = Environment.maxFileSize

        //Show the spinny image when an Ajax call starts
        LiftRules.ajaxStart =
            Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

        // Make the spinny image go away when it ends
        LiftRules.ajaxEnd =
            Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

        // Force the request to be UTF-8
        LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

        // What is the function to test if a user is logged in?
        LiftRules.loggedInTest = Full(() => User.loggedIn_?)

        LiftRules.dateTimeConverter.default.set(RusDateTimeConverter)

        // Использование HTML5
        LiftRules.htmlProperties.default.set((r: Req) =>
            new Html5Properties(r.userAgent))

        LiftRules.dispatch.append(BookRestService)

        // HTTP Кэширование
        val defaultHeaders = LiftRules.defaultHeaders
        LiftRules.defaultHeaders = {
            case (_, Req("book" :: _ :: _, _, _)) => Nil
            case any => defaultHeaders(any)
        }

        // Make a transaction span the whole HTTP request
        S.addAround(DB.buildLoanWrapper)

        // Монитор сессий
        SessionMaster.sessionWatchers =
            SessionMonitor :: SessionMaster.sessionWatchers

        // Инициализация объектов
        BookProcessor.init
    }
}
