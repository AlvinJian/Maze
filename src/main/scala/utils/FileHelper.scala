package utils

import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.ImageWriter

import scala.util.{Failure, Success, Try}

object FileHelper {
  def saveToFile(image: ImmutableImage, writer: ImageWriter,
                 filename: String, directoryName: String = ""): Try[File] = {
    var imageFile: Try[File] =
      if (directoryName.length > 0) {
        val dir = new File(directoryName)
        if (dir.exists() && !dir.isDirectory) {
          Failure(new RuntimeException(s"${directoryName} is an existing file not a directory"))
        } else if (!dir.exists() && !dir.mkdir()) {
          Failure(new RuntimeException(s"Directory: ${directoryName} creation failed"))
        }
        Success(new File(dir, filename))
      } else {
        Success(new File(filename))
      }

    if (imageFile.isSuccess) {
      try {
        image.output(writer, imageFile.get)
      } catch {
        case e: Throwable => imageFile = Failure(e)
      }
    }
    imageFile
  }
}
