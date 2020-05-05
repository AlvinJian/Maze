package utils

import java.io.File
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.ImageWriter

object FileHelper {
  def saveToFile(image: ImmutableImage, writer: ImageWriter,
                 filename: String, directoryName: String = ""): Unit = {
    val imageFile =
      if (directoryName.length > 0) {
        val dir = new File(directoryName)
        if (dir.exists() && !dir.isDirectory) {
          throw new RuntimeException(s"${directoryName} is an existing file not a directory")
        } else if (!dir.exists() && !dir.mkdir()) {
          throw new RuntimeException(s"Directory: ${directoryName} creation failed")
        }
        new File(dir, filename)
      } else {
        new File(filename)
      }

    image.output(writer, imageFile)
  }
}
