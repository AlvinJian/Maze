package playground

import java.io.File

import algorithmex.{DistanceMap, HuntAndKillMaze, RecurBackTrackMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.implicits._
import image.{Drawer, GifCreator, MazeImage}
import maze.{PolarMaze, Position2D, WeaveMaze}
import utils.FileHelper

import scala.swing.FileChooser
import scala.util.{Failure, Random, Success, Try}

// copy the code here and create a new scala class deriving from App in
// playground package. The code under playground is not tracked except
// for this `Template` class
object Template extends App {
  def setupWorkspace: Option[File] = {
    val jc: FileChooser = new FileChooser(new File("."))
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
  val optWs = setupWorkspace
  if (optWs.isEmpty) System.exit(0)
  val dir: File = optWs.get
  val cellSize: Int = 32
  val padding: Int = 2

  var maze = PolarMaze(15)
  val mazeName = maze.info.name
  maze = RecurBackTrackMaze.generate(rand, maze)
  val funcDrawMaze = MazeImage.func(maze)
  var image = funcDrawMaze(cellSize, padding, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
  var fileRes = FileHelper.saveToFile(image, s"${mazeName}_RecurBackTrackMaze$ext", dir)
  fileRes.fold(fail => System.err.println(fail),
    file => println(s"${file.getPath} is written successfully"))

  val mapRes = DistanceMap.maximize(Position2D(0, 0), maze)
  if (mapRes.isEmpty) {
    System.err.println("fail to create distance map")
    System.exit(1)
  }
  val distanceMap = mapRes.get
  val path = distanceMap.pathTo(distanceMap.max._1)
  if (path.isEmpty) {
    System.err.println("path is empty")
    System.exit(1)
  }

  val funcPathColor: Position2D => RGBColor = p => {
    if (path.contains(p)) Drawer.distToColor(distanceMap, p)
    else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
  }
  image = funcDrawMaze(cellSize, padding, funcPathColor)
  fileRes = FileHelper.saveToFile(image, s"${mazeName}_RecurBackTrackMaze_Path$ext", dir)
  fileRes.fold(fail => System.err.println(fail),
    file => println(s"${file.getPath} is written successfully"))

  val funcDrawMazePerFrame: Position2D => ImmutableImage = curPos => {
    val slice = path.slice(0, path.indexOf(curPos)).toSet
    val funcColor: Position2D => RGBColor = pos => {
      if (pos == curPos) RGBColor.fromAwt(java.awt.Color.YELLOW)
      else if (slice.contains(pos)) Drawer.distToColor(distanceMap, pos)
      else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
    }
    funcDrawMaze(cellSize, padding, funcColor)
  }
  val gifCreator = new GifCreator[Position2D](path, funcDrawMazePerFrame)
  val gifRes = gifCreator.produce(150, dir, s"${mazeName}_RecurBackTrackMaze_Path_Anim.gif")
  gifRes match {
    case Failure(exception) => System.err.println(exception)
    case Success(file) => println(s"$file is written successfully")
  }
}
