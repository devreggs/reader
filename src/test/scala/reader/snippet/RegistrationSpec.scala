package reader.snippet

import org.specs2._

class RegistrationSpec extends mutable.SpecificationWithJUnit {

    "anon" should {
        "be start from anon" in {
            "anon" must startWith("anon")
        }
    }
}
