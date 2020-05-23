import algorithm.{AldousBroderMaze, BinaryTreeMaze, DistanceEx, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import grid.{CellEx, GraphEx, GridEx}
import org.scalatest.FunSuite
import utils.{ColoredImageCreator, FileHelper, ImageCreator, MazeImageCreator, Background}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val dir: String = "images"

  val rand: Random = new Random(System.currentTimeMillis())
  val cellSize: Int = 32
  val padding: Option[Int] = Some(5)

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
    val grid = new GridEx(20,20)
    val generator = new BinaryTreeMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
    val start = grid(0, 0)
    val distMap = DistanceEx.from(maze, start).get
    val end = grid((grid.row-1)/2, (grid.col-1)/2)
    distMap.pathTo(end) match {
      case Some(path) => pathCheck(path, distMap, start, end)
      case None => assert(false)
    }
    val img = ImageCreator.create(maze, cellSize = 32, padding = Some(5))
    val file = FileHelper.saveToFile(img, writer, s"BinaryTree${ext}", "images")
    assert(file.isSuccess)
  }

  test("Algorithm.SidewinderEx") {
    val grid = new GridEx(20,20)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val start: CellEx = grid(0, 0)
    val distMap = DistanceEx.from(maze, start).get
    val content = (cell: CellEx) => {
      if (distMap.contains(cell)) distMap(cell).toString else "-"
    }
    print(maze/*.dump(content)*/); println()
    val goal = grid((grid.row-1)/2, (grid.col-1)/2)
    distMap.pathTo(goal) match {
      case Some(path) => {
        val pathSet = path.toSet;
        print(maze.dump((c: CellEx) =>
          if (pathSet.contains(c)) content(c) else " " ))
        pathCheck(path, distMap, start, goal)
      }
      case None => assert(false)
    }
    val img = ImageCreator.create(maze, cellSize=cellSize, padding=padding)
    val file = FileHelper.saveToFile(img, writer, s"Sidewinder$ext", "images")
    assert(file.isSuccess)
  }

  test("Algorithm.AldousBroder") {
    val grid = new GridEx(20, 20)
    val generator = new AldousBroderMaze(rand)
    val maze = generator.generate(grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"AldousBroder$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze,
      grid((grid.row-1)/2, (grid.col-1)/2)).get
    image = ImageCreator.create(distMap, cellSize, padding)
    f = FileHelper.saveToFile(image, writer, s"AldousBroder_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.Wilson") {
    val grid = new GridEx(20, 20)
    val generator = new WilsonMaze(rand)
    val maze = generator.generate(grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"Wilson$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.HuntAndKill") {
    val grid = new GridEx(20, 20)
    val generator = new HuntAndKillMaze(rand)
    val maze = generator.generate(grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"HuntAndKill$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = ImageCreator.create(distMap, cellSize, padding)
    f = FileHelper.saveToFile(image, writer, s"HuntAndKill_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.RecursiveBackTrack") {
    val grid = new GridEx(20, 20)
    val generator = new RecurBackTrackMaze(rand)
    val maze = generator.generate(grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = ImageCreator.create(distMap, 32, Some(5))
    f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.LongestPath") {
    val grid = new GridEx(20,20)
    val generator = new RecurBackTrackMaze(rand)
    val maze = generator.generate(grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"LongestPath_RecurBackTrackMaze$ext", dir)
    assert(f.isSuccess)
    val thru: CellEx = grid((grid.row-1)/2, (grid.col-1)/2)
    val distMap = DistanceEx.createMax(maze, thru).get
    val (farCell, maxDist) = distMap.max
    val ref = grid.maxBy((c) => distMap(c))
    val testMaxDist = distMap(ref)
    val testMaxCells = grid.filter((c) => distMap(c) == testMaxDist).toSet
    assert(testMaxDist == maxDist)
    assert(testMaxCells.contains(farCell))
    assert(distMap(farCell) == maxDist)
    image = ImageCreator.create(distMap, cellSize, padding)
    f = FileHelper.saveToFile(image, writer, s"LongestPath_MaxDistMap$ext", dir)
    assert(f.isSuccess)
    val path = distMap.pathTo(farCell).get
    pathCheck(path, distMap, distMap.root, farCell)
    val pathSet = path.toSet
    println(maze.dump((c) => {
      if (pathSet.contains(c)) distMap(c).toString else ""
    }))
    val colorMapper: (CellEx)=>RGBColor = (c: CellEx) => {
      if (pathSet.contains(c)) distMap.colorMapper(c)
      else RGBColor.fromAwt(java.awt.Color.GRAY)
    }
    image = ImageCreator.batch(padding)(
      new ColoredImageCreator(grid, cellSize, colorMapper),
      new MazeImageCreator(maze, cellSize),
    )
    f = FileHelper.saveToFile(image, writer, s"LongestPath$ext", dir)
    assert(f.isSuccess)
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
