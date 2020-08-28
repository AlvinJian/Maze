import algorithmex.{AldousBroderMaze, BinaryTreeMaze, DistanceMap, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import image.{Drawer, MazeImage}
import maze.{Cell2D, Cell2DRect, Maze, PolarMaze, PolarMazeInfo, Position2D, RectMaze, RectMazeInfo}
import org.scalatest.FunSuite
import utils.FileHelper

import scala.annotation.tailrec
import scala.util.Random

class MazeAlgoTest extends FunSuite {
  val rand: Random = new Random()
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val dir: String = "images"

  def pathCheck(path: List[Position2D], distMap: DistanceMap,
                start: Position2D, end: Position2D): Unit = {
    assert(path.head == start)
    assert(path.last == end)
    val maze = distMap.info.maze
    @tailrec
    def loopCheck(list: List[Position2D]): Unit = list match {
      case ::(head, next) => {
        if (next.nonEmpty) {
          val curPos = head
          val nextPos = next.head
          assert(maze.linkedBy(curPos).map(c => c.pos).contains(nextPos))
          assert(distMap.data(head)+1 == distMap.data(nextPos))
        }
        loopCheck(next)
      }
      case Nil => ()
    }
    loopCheck(path)
  }

  test("Aldous") {
    var rectMaze = RectMaze(10, 10)
    rectMaze = AldousBroderMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_Aldous$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("Sidewinder") {
    var rectMaze = RectMaze(10,10)
    rectMaze = SidewinderMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_Sidewinder$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("BinaryTree") {
    var rectMaze = RectMaze(10,10)
    rectMaze = BinaryTreeMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_BinaryTree$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("RecursiveBackTrack") {
    var rectMaze = RectMaze(10,10)
    rectMaze = RecurBackTrackMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_RecurBT$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("HuntAndKill") {
    var rectMaze = RectMaze(10, 10)
    rectMaze = HuntAndKillMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_HuntAndKill$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("Wilson") {
    var rectMaze = RectMaze(10, 10)
    rectMaze = WilsonMaze.generate(rand, rectMaze)
    val func = MazeImage.func(rectMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewMaze_Wilson$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("Wilson.PolarMaze") {
    var polarMaze = PolarMaze(10)
    polarMaze = WilsonMaze.generate(rand, polarMaze)
    val func = MazeImage.func(polarMaze)
    var image = func(32, 4, _=>RGBColor.fromAwt(java.awt.Color.WHITE))
    var fileRes = FileHelper.saveToFile(image, writer, s"NewPolarMaze_Wilson$ext", dir)
    assert(fileRes.isSuccess)
  }

  test("DistMap") {
    val rectMaze = RectMaze(10, 10)
    val polarMaze = PolarMaze(5)
    val inputMazes: List[Maze[Cell2D]] = List(rectMaze, polarMaze)

    for (inMaze <- inputMazes) {
      val genMazes: List[(Maze[Cell2D], String)] = inMaze.info match {
        case RectMazeInfo(_, rectMaze) => List(AldousBroderMaze, SidewinderMaze, BinaryTreeMaze, RecurBackTrackMaze,
          HuntAndKillMaze, WilsonMaze).map(gen => (gen.generate(rand, rectMaze), gen.getClass.getSimpleName))
        case _ => List(AldousBroderMaze, RecurBackTrackMaze, HuntAndKillMaze, WilsonMaze).map {
          gen => (gen.generate(rand, inMaze), gen.getClass.getSimpleName)
        }
      }
      for (tup <- genMazes) {
        val (genMaze, genName) = tup
        DistanceMap.tryCreate(genMaze.randomCell(rand).pos, genMaze) match {
          case Some(distanceMap) => {
            val start = distanceMap.root
            val (end, maxDist) = distanceMap.max
            val path = distanceMap.pathTo(end)
            assert(path.nonEmpty)
            assert(maxDist == distanceMap.data(path.last))
            pathCheck(path, distanceMap, start, end)
            println(s"${genMaze.info.name} ${genName} ${maxDist}")
          }
          case None => assert(false)
        }
      }
    }
  }
}
