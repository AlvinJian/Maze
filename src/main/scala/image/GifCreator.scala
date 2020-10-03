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

  def produce(interval: Long, target: File): Try[File] = {
    val duration = futureImages.size.asInstanceOf[Long] * interval + interval/2
    val writer = new StreamingGifWriter(java.time.Duration.ofMillis(duration), true)
    val stream = writer.prepareStream(target, BufferedImage.TYPE_INT_ARGB)
    val res = futureImages.foldLeft(Try(stream))((prevStream, futureImage) => {
      val image = if (prevStream.isSuccess) Await.result(futureImage, 60 seconds)
        else ImmutableImage.create(0, 0)
      prevStream.flatMap(s => Try(s.writeFrame(image, java.time.Duration.ofMillis(interval))))
    })
    res.map(s => {
      s.close()
      target
    })
  }

  def produce(interval: Long, dir: File, filename: String): Try[File] = {
    if (dir.exists() && dir.isDirectory) {
      produce(interval, new File(dir, filename))
    } else Failure(new RuntimeException("Failed to create image file"))
  }
}
