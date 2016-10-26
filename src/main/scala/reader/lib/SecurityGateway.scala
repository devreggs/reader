package reader.lib

import reader.model._
import _root_.net.liftweb.common._
import net.liftweb.http.S

object SecurityGateway extends Logger  {

    def allowRead(book: Book) = {
        (book.state.get == BookState.published) ||
            (book.owner.get == User.currentUser.id.is) ||
            (User.currentUser.role == UserRole.admin) ||
            User.currentUser.books.find(_.id.get == book.id.get).isDefined
    }

    def allowChangeUser(user: User) = {
        User.currentUser.id.is == user.id.get ||
            User.currentUser.role == UserRole.admin
    }

    def allowRemove(book: Book) = {
        (book.owner.get == User.currentUser.id.is) ||
            (User.currentUser.role == UserRole.admin)
    }

    def allowUpload = {
        User.currentUser.role == UserRole.reader ||
            User.currentUser.role == UserRole.admin
    }

    def allowAdmin = User.currentUser.role == UserRole.admin

    def allowCabinet = User.currentUser.role != UserRole.anon
}
