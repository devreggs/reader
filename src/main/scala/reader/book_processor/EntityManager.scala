package reader.book_processor

import java.io.{File, FileInputStream}

import scala.xml.{EntityResolver, InputSource}

/**
  * Created by bw-ok on 22.06.2017.
  */
class EntityManager extends EntityResolver {
    val recourcesDir = getClass.getResource("/xhtml11").getPath.toString + File.separator
    def resolveEntity(publicId: String, systemId: String ): InputSource = {
        /* code goes here to return contents of DTD */
        if (publicId == "-//W3C//DTD XHTML 1.1//EN")
            new InputSource(new FileInputStream(recourcesDir + "xhtml11.dtd"))
        else if (publicId.startsWith("-//W3C//ELEMENTS XHTML") || publicId.startsWith("-//W3C//ENTITIES"))
            new InputSource(new FileInputStream(recourcesDir + systemId.substring(systemId.lastIndexOf("/") + 1)))
        else
            new InputSource(systemId)
    }

}
