package playground

import algorithmex.{HuntAndKillMaze, WilsonMaze}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.implicits
import image.MazeImage
import maze.PolarMaze
import utils.FileHelper

import scala.util.Random

object ScratchPad extends App {
  val rand: Random = new Random()
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val dir: String = "images"

  var polarMaze = PolarMaze(10)
  polarMaze = HuntAndKillMaze.generate(rand, polarMaze)
  val func = MazeImage.func(polarMaze)
  var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
  var fileRes = FileHelper.saveToFile(image, writer, s"NewPolarMaze_HuntAndKill$ext", dir)
  assert(fileRes.isSuccess)
}
