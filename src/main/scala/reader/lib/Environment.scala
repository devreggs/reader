package reader.lib

import reader.model._
import net.liftweb.mapper.ByList
import HelpSnippets._
import org.apache.commons.io.FileUtils
import java.io._
import net.liftweb.http.LiftRules
import net.liftweb.common.{Full, Logger}
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator

object Environment {

    val demoBookId1 = EnvironmentValue("demoBookId1", "")

    val demoBookId2 = EnvironmentValue("demoBookId2", "")

    val demoBookId3 = EnvironmentValue("demoBookId3", "")

    val maxSessionAge = 30 * 24 * 60 * 60

    val maxFileSize = 45 * 1024 * 1024

    val maxThreads = 8

    val defaultPath = EnvironmentValue("defaultPath",
                                        if(System.getenv("OPENSHIFT_DATA_DIR") == null)
                                            "C:/workspace/storage"
                                        else
                                            System.getenv("OPENSHIFT_DATA_DIR"))

    def demoBooks = {
        val ids = demoBookId1.value.get.toLongOr(-1) ::
            demoBookId2.value.get.toLongOr(-1) ::
            demoBookId3.value.get.toLongOr(-1) :: Nil
        Book.findAll(ByList(Book.id, ids))
    }

    def storageSize = {
        FileUtils.sizeOfDirectory(new File(defaultPath.value.get))
    }
}
