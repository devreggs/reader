package reader
package model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.common.Full
import java.util.Date
import net.liftweb.http.{SessionVar, S}
import net.liftweb.http.provider.HTTPCookie
import reader.lib.Environment
import reader.lib.HelpSnippets._
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.immutable.HashMap

object UserRole extends Enumeration {
    val anon, reader, admin = Value
}

class User extends LongKeyedMapper[User]
with OneToMany[Long, User]
with IdPK
with ManyToMany {

    def getSingleton = User

    /**
     * Техническая информация
     */

    object login extends MappedString(this, 255)

    object email extends MappedEmail(this, 255)

    object password extends MappedString(this, 255)

    object role extends MappedEnum(this, UserRole) {
        override def defaultValue = UserRole.reader
    }

    object created extends MappedDateTime(this) {
        override def defaultValue = new Date()
    }

    object lastConnection extends MappedDateTime(this) {
        override def defaultValue = new Date()
    }

    object spaceSize extends MappedLong(this) {
        override def defaultValue = 20000000
    }

    object ownBooks extends MappedOneToMany(Book, Book.owner)

    object books extends MappedManyToMany(BookLink, BookLink.user, BookLink.book, Book)

    object fontSize extends MappedString(this, 5) {
        override def defaultValue = "12pt"
    }

    object fontFamily extends MappedString(this, 60) {
        override def defaultValue = "Georgia"
    }

    object textAlign extends MappedString(this, 10) {
        override def defaultValue = "center"
    }

    object theme extends MappedString(this, 24) {
        override def defaultValue = "white"
    }

    override def delete_! = {
        BookLastPosition.removeFor(this)
        super.delete_!
    }

    def myGenres = {
        books.flatMap(book => book.genres).distinct
    }

    def myAuthors = {
        books.flatMap(book => book.authors).distinct
    }

    def rawAndFailed = {
        ownBooks.refresh
        ownBooks.filter(book => book.state.get == BookState.raw || book.state.get == BookState.wrong)
    }

    def freeSpace = {
        ownBooks.refresh
        spaceSize.get - ownBooks.foldLeft(0L)(_ + _.fileSize("original"))
    }

    override def saveMe = {
        if(role != UserRole.anon)
            super.saveMe
        else
            this
    }

    def mutex = User.getMutex(id.get)

}

object User extends User with LongKeyedMetaMapper[User] {

    protected val mutexMapMutex = ""
    protected var mutexMap = HashMap[Long, String]()

    def getMutex(userId: Long) = {
        if(mutexMap.get(userId).isEmpty)
            mutexMapMutex.synchronized{
                mutexMap += (userId -> "")
            }
        mutexMap.get(userId).get
    }

    override def dbTableName = "users"

    def initUser =
        UserSessionLink.byId(S.findCookie("s")
            .openOr(HTTPCookie("s", "-1")).value
            .openOr("-1")
            .toIntOr(-1))
            .openOr(UserSessionLink.create)
            .user.foreign.openOr(createAnon)


    def createAnon = User.create.email("anon@anon").role(UserRole.anon)

    private object sessUser extends SessionVar[User](initUser)

    private object sessMessage extends SessionVar[Box[String]](Empty)

    def currentUser = sessUser.is

    def setCurrentSessionMessage(msg: String) = sessMessage(Full(msg))

    def currentSessionMessage = {
        val result = sessMessage.is
        sessMessage(Empty)
        result
    }

    def updateLastConnection = currentUser.lastConnection(new Date()).saveMe()

    def emailExist(email: String) = User.find(By(User.email, email)).isDefined

    def loggedIn_? = {
        User.currentUser.role != UserRole.anon
    }

    def tryLogIn(email: String, password: String) = {
        User.find(By(User.email, email),
            By(User.password, password)) match {
            case Full(user) => {
                logIn(user)
                true
            }
            case _ => false
        }
    }

    def logIn(user: User) {
        sessUser(user)
        val usl = UserSessionLink.create.user(user.id.get).saveMe()
        val c = HTTPCookie("s", usl.id.get.toString).setMaxAge(Environment.maxSessionAge)
        S.addCookie(c)
        User.updateLastConnection
    }

    def logOut = {
        if(S.findCookie("s").isDefined) {
            val c = S.findCookie("s").get
            val usl = UserSessionLink.byId(c.value.openOr("-1").toIntOr(-1))
            if(usl.isDefined)
                usl.get.delete_!
            S.deleteCookie("s")
        }
        User.updateLastConnection
        sessUser(createAnon)
    }

    def restorePassword(email: String) = {
    }

    def byEmail(email: String) = User.find(By(User.email, email))

    def byId(id: Long) = find(By(User.id, id))
}
