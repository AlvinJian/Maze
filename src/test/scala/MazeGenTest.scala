import algorithm.{BinaryTreeMaze, DistanceEx, SidewinderMaze}
import com.sksamuel.scrimage.nio.PngWriter
import grid.{CellEx, GraphEx, GridEx}
import org.scalatest.FunSuite
import utils.FileHelper

import scala.util.{Failure, Random, Success}

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())

  def pathCheck(path: List[CellEx], distMap: DistanceEx,
                start: CellEx, end: CellEx): Unit = {
    assert(path.head == start)
    assert(path.last == end)
    val graph = distMap.graph
    for (i <- 0 until path.size-1) {
      assert(graph.isLinked(path(i), path(i+1)))
      assert(distMap(path(i))+1 == distMap(path(i+1)))
    }
  }

  test("Algorithm.BinaryTreeEx") {
    val grid = new GridEx(8,10)
    val generator = new BinaryTreeMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
    val start = grid(7, 0)
    val distMap = DistanceEx.from(maze, start).get
    val end = grid(7, 5)
    distMap.pathTo(end) match {
      case Some(path) => pathCheck(path, distMap, start, end)
      case None => assert(false)
    }
    val img = maze.toImage(32)
    val file = FileHelper.saveToFile(img, writer, s"BinaryTree${ext}", "images")
    assert(file.isSuccess)
  }

  test("Algorithm.SidewinderEx") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val start: CellEx = grid(7, 0)
    val distMap = DistanceEx.from(maze, start).get
    val content = (cell: CellEx) => {
      if (distMap.contains(cell)) distMap(cell).toString else "-"
    }
    print(maze/*.dump(content)*/); println()
    val goal = grid(7, 5)
    distMap.pathTo(goal) match {
      case Some(path) => {
        val pathSet = path.toSet;
        print(maze.dump((c: CellEx) =>
          if (pathSet.contains(c)) content(c) else " " ))
        pathCheck(path, distMap, start, goal)
      }
      case None => assert(false)
    }
    val img = maze.toImage(32)
    val file = FileHelper.saveToFile(img, writer, s"Sidewinder$ext", "images")
    assert(file.isSuccess)
  }

  test("Algorithm.LongestPath") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val mazeImg = maze.toImage(32)
    val f = FileHelper.saveToFile(mazeImg, writer, s"Sidewinder$ext", "images")
    assert(f.isSuccess)
    val start: CellEx = grid(7, 0)
    var distMap = DistanceEx.from(maze, start).get
    val (newStart, _) = distMap.max
    distMap = DistanceEx.from(maze, newStart).get
    val (farCell, maxDist) = distMap.max
    val ref = grid.maxBy((c) => distMap(c))
    val testMaxDist = distMap(ref)
    val testMaxCells = grid.filter((c) => distMap(c) == testMaxDist).toSet
    assert(testMaxDist == maxDist)
    assert(testMaxCells.contains(farCell))
    assert(distMap(farCell) == maxDist)
    val path = distMap.pathTo(farCell).get
    pathCheck(path, distMap, newStart, farCell)
    val pathSet = path.toSet
    println(maze.dump((c) => {
      if (pathSet.contains(c)) distMap(c).toString else ""
    }))
    val image = distMap.toImage(32)
    val file = FileHelper.saveToFile(image, writer, s"DistanceMap$ext", "images")
    file match {
      case Failure(exception) => exception.printStackTrace()
      case Success(value) =>
    }
    assert(file.isSuccess)
  }
}
