package reader.book_processor

import scala.sys.process._
import java.io.File
import java.nio.file.{StandardCopyOption, Paths, Files}

private object Fb2Fix extends net.liftweb.common.Logger{

    def apply(sourceFilename : String): Boolean = {
        val sourcePath = Paths.get(sourceFilename)

        val sourceFilenameExtended = sourceFilename + ".fb2"
        val sourceFileNameExtended = new File(sourceFilenameExtended).getName
        val sourcePathExtended = Paths.get(sourceFilenameExtended)

        val outputPath = Paths.get(Paths.get(sourceFilename).getParent.toString + File.separator + "fb2fixer_tmp")
        val outputFoldername = outputPath.toString

        val goodPath = Paths.get(outputFoldername + File.separator + "Good" + File.separator + sourceFileNameExtended)
        val badPath = Paths.get(outputFoldername + File.separator + "Bad" + File.separator + sourceFileNameExtended)

        try {
            Files.deleteIfExists(goodPath)
            Files.deleteIfExists(badPath)

            Files.copy(sourcePath, sourcePathExtended)

            val process = Seq("fb2fix",
                "/compress-",
                "/output:\"%s\"" format outputFoldername,
                sourceFilenameExtended)

            // run the process catching it's output as a string; if non-zero exit code returned the exception is thrown
            info(process.!!)

            val success =
            {
                if (Files.exists(goodPath)){
                    Files.copy(goodPath, sourcePath, StandardCopyOption.REPLACE_EXISTING)
                    true
                }
                else if (Files.exists(badPath)){
                    info(s"$sourceFilename was bad fb2")
                    false
                }
                else {
                    info(s"$sourceFilename was nor good neither bad")
                    false
                }
            }

            success
        } catch {
            case e: Exception => {
                error(e.getMessage)
                debug(e.getStackTraceString)
                false
            }
        } finally {
            Files.deleteIfExists(goodPath)
            Files.deleteIfExists(badPath)
            Files.deleteIfExists(sourcePathExtended)
        }
    }
}