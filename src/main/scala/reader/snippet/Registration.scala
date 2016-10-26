package reader.snippet


import net.liftweb.util._
import reader.model._
import Helpers._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.provider.HTTPCookie

class Registration {

    private object email extends RequestVar("")
    private object password extends RequestVar("")
    private object passwordCheck extends RequestVar("")

    def submitRegistrationForm() = {
        if (User.emailExist(email))
            S.error("message", "Пользователь с таким e-mail уже зарегистрирован")
        else if (password.get != passwordCheck.get)
            S.error("message", "Пароли не совпадают")
        else if (password.get.length() < 6)
            S.error("message", "Пароль должен быть длиннее 6 символов")
        else {
            val user = User.create.email(email).password(password).saveMe()
            reader.lib.Environment.demoBooks.foreach(BookLink.create.book(_).user(user).saveMe())
            User.logIn(user)
            //S.redirectTo("index")
        }
    }

    def accountMenu = {
        "#currentLogin *" #> User.currentUser.email.get
    }

    def submitLoginForm() = {
        if(!User.tryLogIn(email, password))
            S.error("message", "Неправильный адрес электронной почты или пароль")
    }

    def submitRestorePasswordForm() = {
        User.restorePassword(email)
        S.notice("message", "На введённый адрес электронной почты выслано письмо с паролем")
    }

    def registrationForm = {
        "name=input-email" #> SHtml.text(email, x => email(x)) &
            "name=input-password" #> SHtml.password(password, x => password(x)) &
            "name=input-password-check" #> SHtml.password(passwordCheck, x => passwordCheck(x)) &
            "type=submit" #> SHtml.onSubmitUnit(() => {
                submitRegistrationForm()
            })
    }

    def loginForm = {
        "name=input-email" #> SHtml.text(email, x => email(x)) &
            "name=input-password" #> SHtml.password(password, x => password(x)) &
            "type=submit" #> SHtml.onSubmitUnit(() => {
                submitLoginForm()
            })
    }

    def restorePasswordForm = {
        "name=input-email" #> SHtml.text("", x => email(x)) &
            "type=submit" #> SHtml.onSubmitUnit(() => {
                submitRestorePasswordForm()
            })
    }

}
