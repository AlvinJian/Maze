import algorithm.{AldousBroderMaze, BinaryTreeMaze, DistanceEx, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.PngWriter
import grid.{CellEx, GraphEx, GridEx}
import org.scalatest.FunSuite
import utils.FileHelper

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())
  val dir: String = "images"

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

  test("Algorithm.AldousBroder") {
    val grid = new GridEx(8, 10)
    val generator = new AldousBroderMaze(rand)
    val maze = generator.generate(grid)
    var image = maze.toImage(32)
    var f = FileHelper.saveToFile(image, writer, s"AldousBroder$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(3, 4)).get
    f = FileHelper.saveToFile(distMap.toImage(32), writer,
                              s"AldousBroder_DistMap$ext", dir)
    assert(f.isSuccess)
    image = distMap.pathToAsImage(64, distMap.max._1).get
    f = FileHelper.saveToFile(image, writer, s"AldousBroder_MaxPath$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.Wilson") {
    val grid = new GridEx(8, 10)
    val generator = new WilsonMaze(rand)
    val maze = generator.generate(grid)
    var image = maze.toImage(32)
    var f = FileHelper.saveToFile(image, writer, s"Wilson$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.HuntAndKill") {
    val grid = new GridEx(20, 20)
    val generator = new HuntAndKillMaze(rand)
    val maze = generator.generate(grid)
    var image = maze.toImage(32)
    var f = FileHelper.saveToFile(image, writer, s"HuntAndKill$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = distMap.toImage()
    f = FileHelper.saveToFile(image, writer, s"HuntAndKill_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.RecursiveBackTrack") {
    val grid = new GridEx(20, 20)
    val generator = new RecurBackTrackMaze(rand)
    val maze = generator.generate(grid)
    var image = maze.toImage(10)
    var f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = distMap.toImage()
    f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.LongestPath") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val mazeImg = maze.toImage(32)
    val f = FileHelper.saveToFile(mazeImg, writer, s"Sidewinder$ext", "images")
    assert(f.isSuccess)
    val start: CellEx = grid(7, 0)
    val distMap = DistanceEx.createMax(maze, start).get
    val (farCell, maxDist) = distMap.max
    val ref = grid.maxBy((c) => distMap(c))
    val testMaxDist = distMap(ref)
    val testMaxCells = grid.filter((c) => distMap(c) == testMaxDist).toSet
    assert(testMaxDist == maxDist)
    assert(testMaxCells.contains(farCell))
    assert(distMap(farCell) == maxDist)
    distMap.pathToAsImage(32, farCell) match {
      case Some(pathImg) => {
        val imgFile = FileHelper.saveToFile(pathImg, writer, s"longestPath$ext", "images")
        assert(imgFile.isSuccess)
      }
      case None => assert(false)
    }
    val path = distMap.pathTo(farCell).get
    pathCheck(path, distMap, distMap.root, farCell)
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

  test("Algorithm.DeadEndTest") {
    val algorithms = List[MazeGenerator](
      new AldousBroderMaze(rand), new BinaryTreeMaze(rand), new HuntAndKillMaze(rand),
      new SidewinderMaze(rand), new RecurBackTrackMaze(rand), new WilsonMaze(rand))
    var averages: Map[MazeGenerator, Int] = Map()
    val size = 20
    val grid = new GridEx(size,size)
    val tries = 100
    for (algo <- algorithms) {
      val counts = mutable.Seq[Int]() ++ (for (_ <- 0 until tries) yield 0)
      for (i <- 0 until tries) {
        val maze = algo.generate(grid)
        counts(i) = maze.deadEnds.size
      }
      val total: Int = counts.sum
      averages = averages + (algo -> (total/counts.size))
    }
    val orders = averages.keys.toArray.sortBy((a)=>averages(a))
    println("average deadends statistics:")
    for (algo <- orders) {
      val percent: Double = averages(algo).toDouble * 100.0 / (size * size).toDouble
      println(s"${algo.getClass.getSimpleName}: ${averages(algo)} (${percent}%)")
    }
  }
}
