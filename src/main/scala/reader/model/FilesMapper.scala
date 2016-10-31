package reader.model

import net.liftweb.http.FileParamHolder
import reader.lib.Environment
import java.io._
import java.net.{URLConnection, URL}
import _root_.net.liftweb.common._
import org.apache.commons.io.{IOUtils, FileUtils}
import net.liftweb.mapper.{LongKeyedMapper, BaseKeyedMapper, BaseLongKeyedMapper, IdPK}

// Для сущностей с данной чертой создется папка в хранилище (/subpath/primaryKey)
trait FilesMapper extends Logger with BaseKeyedMapper {

    def getId = primaryKeyField.get.toString

    def directoryName = primaryKeyField.fieldOwner.getSingleton.dbTableName

    def directoryPath = Environment.defaultPath.value.get + "/" + directoryName + "/" + getId

    def filePath(name: String) = {
        directoryPath + "/" + name
    }

    def fileExist(fileName: String) = {
        (new File(filePath(fileName))).exists()
    }

    def directorySize = {
        new File(directoryPath).mkdirs()
        FileUtils.sizeOfDirectory(new File(directoryPath))
    }

    def fileSize(name: String) = {
        if(fileExist(name))
            FileUtils.sizeOf(new File(filePath(name)))
        else
            0L
    }

    def uploadFile(name: String, fph: FileParamHolder) = {
        new File(directoryPath).mkdirs()
        info("create " + directoryPath)
        val storeFile = new FileOutputStream(new File(directoryPath + "/" + name))
        storeFile.write(fph.file)
        storeFile.close()
        true
    }

    def readFile(filename: String) = {
        val buffer = scala.io.Source.fromFile(filePath(filename))("UTF-8")
        val lines = buffer.getLines.reduceLeft(_ + _)
        buffer.close()
        lines
    }

    def saveFileInputStream(name: String, origFilename: String, inputStream: InputStream) = {
        new File(directoryPath).mkdirs()
        val entryDestination = new File(filePath(name))
        val outStream = new FileOutputStream(entryDestination)
        IOUtils.copy(inputStream, outStream)
        IOUtils.closeQuietly(inputStream)
        IOUtils.closeQuietly(outStream)
        true
    }

    class FileMapperDownloadException extends Exception

    def downloadFile(name: String, url: String) {
        new File(directoryPath).mkdirs()
        FileUtils.copyURLToFile(new URL(url), new File(directoryPath + "/" + name))
    }

    def removeFile(name: String) {
        if(fileExist(name))
            FileUtils.forceDelete(new File(filePath(name)))
    }

    def removeFiles = {
        new File(directoryPath).mkdirs()
        FileUtils.deleteDirectory(new File(directoryPath))
    }
}

