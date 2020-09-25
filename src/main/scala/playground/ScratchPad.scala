package playground

import java.io.File

import algorithmex.{DistanceMap, HuntAndKillMaze, WilsonMaze}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.implicits._
import image.{Drawer, MazeImage}
import maze.{PolarMaze, Position2D, WeaveMaze}
import utils.FileHelper

import scala.swing.FileChooser
import scala.util.{Random, Try}

object ScratchPad extends App {
  def setupWorkspace: Option[File] = {
    val jc: FileChooser = new FileChooser()
    val title: String = "select a directory as a workspace:"
    jc.title = title
    jc.multiSelectionEnabled = false
    jc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
    val ret = jc.showOpenDialog(null)
    if (ret == FileChooser.Result.Approve) Some(jc.selectedFile)
    else None
  }

  val rand: Random = new Random()
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val wsRes = Try(setupWorkspace.get)
  if (wsRes.isFailure) System.exit(0)
  val dir: File = wsRes.get

  var maze = WeaveMaze(20, 20)
  maze = HuntAndKillMaze.generate(rand, maze)
  val func = MazeImage.func(maze)
  var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
  var fileRes = FileHelper.saveToFile(image, s"NewMaze_HuntAndKill$ext", dir)
  fileRes.fold(fail => System.err.println(fail),
    file => println(s"${file.getPath} is written successfully"))

  val mapRes = Try(DistanceMap.maximize(Position2D(0, 0), maze).get)
  if (mapRes.isFailure) {
    System.err.println("fail to create distance map")
    System.exit(1)
  }
  val distanceMap = mapRes.get
  val path = distanceMap.pathTo(distanceMap.max._1)
  if (path.isEmpty) {
    System.err.println("path is empty")
    System.exit(1)
  }
  image = func(32, 4, p => {
    if (path.contains(p)) Drawer.distToColor(distanceMap, p)
    else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
  })
  fileRes = FileHelper.saveToFile(image, s"NewMaze_HuntAndKill_Path$ext", dir)
  fileRes.fold(fail => System.err.println(fail),
    file => println(s"${file.getPath} is written successfully"))
}
