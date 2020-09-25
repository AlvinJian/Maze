package utils

import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.ImageWriter

import scala.util.{Failure, Success, Try}

import com.sksamuel.scrimage.implicits._

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

    imageFile.flatMap(f => Try(image.output(f)(writer)))
  }

  def saveToFile(image: ImmutableImage, filename: String, directory: File)(implicit writer: ImageWriter): Try[File] = {
    val imageFile: Try[File] = {
      if (directory.exists() && directory.isDirectory) {
        Success(new File(directory, filename))
      } else {
        Failure(new RuntimeException("can't create image file"))
      }
    }
    imageFile.flatMap(f => Try(image.output(f)(writer)))
  }
}
