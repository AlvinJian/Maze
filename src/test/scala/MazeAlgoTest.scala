import algorithmex.{AldousBroderMaze, HuntAndKillMaze, RecurBackTrackMaze, SidewinderMaze}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import image.{Drawer, MazeImage}
import maze.{Cell2D, Cell2DRect, Maze, RectMaze}
import org.scalatest.FunSuite
import utils.FileHelper

import scala.util.Random

class MazeAlgoTest extends FunSuite {
  val rand: Random = new Random()
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val dir: String = "images"

  test("Aldous") {
    var rectMaze = RectMaze(10, 10)
    rectMaze = AldousBroderMaze.generate(rand, rectMaze)
    val func = MazeImage.drawFunc(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_Aldous$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("Sidewinder") {
    var rectMaze = RectMaze(10,10)
    rectMaze = SidewinderMaze.generate(rand, rectMaze)
    val func = MazeImage.drawFunc(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_Sidewinder$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("RecursiveBackTrack") {
    var rectMaze = RectMaze(10,10)
    rectMaze = RecurBackTrackMaze.generate(rand, rectMaze)
    val func = MazeImage.drawFunc(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_RecurBT$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("HuntAndKill") {
    var rectMaze = RectMaze(10, 10)
    rectMaze = HuntAndKillMaze.generate(rand, rectMaze)
    val func = MazeImage.drawFunc(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_HuntAndKill$ext", dir)
    assert(fileRes.isSuccess)
  }
}
