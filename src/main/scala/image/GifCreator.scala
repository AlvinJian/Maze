package image

import java.awt.image.BufferedImage
import java.io.File

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.StreamingGifWriter

import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

// TODO the API is not good enough
class GifCreator[T](input: Seq[T], f: T => ImmutableImage) {
  private val futureImages: Seq[Future[ImmutableImage]] = input.map(t => Future(f(t)))

  def produce(interval: Long, target: File, infLoop: Boolean): Try[Unit] = {
    val duration = if (infLoop) {
      (futureImages.size.asInstanceOf[Long] * interval) * 3
    } else futureImages.size.asInstanceOf[Long] * interval + 10L
    val writer = new StreamingGifWriter(java.time.Duration.ofMillis(duration), infLoop)
    val stream = writer.prepareStream(target, BufferedImage.TYPE_INT_ARGB)
    val res = futureImages.foldLeft(Try(stream))((prevStream, futureImage) => {
      val image = Await.result(futureImage, 60 seconds)
      prevStream.flatMap(s => Try(s.writeFrame(image, java.time.Duration.ofMillis(interval))))
    })
    res.map(s => s.close())
  }

  def produce(interval: Long, dir: File, filename: String, infLoop: Boolean = true): Try[Unit] = {
    if (dir.exists() && dir.isDirectory) {
      produce(interval, new File(dir, filename), infLoop)
    } else Failure(new RuntimeException("Failed to create image file"))
  }
}