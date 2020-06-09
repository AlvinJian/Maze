import algorithm.{AldousBroderMaze, BinaryTreeMaze, DistanceEx, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import grid.{Cell2D, Cell2DCart, GraphEx, GridEx, MaskedGrid, PolarGrid}
import org.scalatest.FunSuite
import utils.{Background, ColoredImageCreator, FileHelper, ImageCreator, MazeImageCreator}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val dir: String = "images"

  val rand: Random = new Random(System.currentTimeMillis())
  val cellSize: Int = 32
  val padding: Option[Int] = Some(5)

  def pathCheck(path: List[Cell2D], distMap: DistanceEx,
                start: Cell2D, end: Cell2D): Unit = {
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
    val maze = BinaryTreeMaze.generate(rand, grid)
    print(maze)
    val start = grid(0, 0)
    val distMap = DistanceEx.from(maze, start).get
    val end = grid((grid.rows-1)/2, (grid.cols-1)/2)
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
    val maze = SidewinderMaze.generate(rand, grid);
    val start: Cell2DCart = grid(0, 0)
    val distMap = DistanceEx.from(maze, start).get
    val content = (cell: Cell2DCart) => {
      if (distMap.contains(cell)) distMap(cell).toString else "-"
    }
    print(maze/*.dump(content)*/); println()
    val goal = grid((grid.rows-1)/2, (grid.cols-1)/2)
    distMap.pathTo(goal) match {
      case Some(path) => {
        val pathSet = path.toSet;
        print(maze.dump((c: Cell2DCart) =>
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
    val maze = AldousBroderMaze.generate(rand, grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"AldousBroder$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze,
      grid((grid.rows-1)/2, (grid.cols-1)/2)).get
    image = ImageCreator.create(distMap, cellSize, padding)
    f = FileHelper.saveToFile(image, writer, s"AldousBroder_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.Wilson") {
    val grid = new GridEx(20, 20)
    val maze = WilsonMaze.generate(rand, grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"Wilson$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.HuntAndKill") {
    val grid = new GridEx(20, 20)
    val maze = HuntAndKillMaze.generate(rand, grid)
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
    val maze = RecurBackTrackMaze.generate(rand, grid)
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
    val maze = RecurBackTrackMaze.generate(rand, grid)
    var image = ImageCreator.create(maze, cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"LongestPath_RecurBackTrackMaze$ext", dir)
    assert(f.isSuccess)
    val thru: Cell2DCart = grid((grid.rows-1)/2, (grid.cols-1)/2)
    val distMap = DistanceEx.createMax(maze, thru).get
    val (farCell, maxDist) = distMap.max
    val ref = grid.maxBy((c) => distMap(c))
    val testMaxDist = distMap(ref)
    val testMaxCells = grid.filter((c) => distMap(c) == testMaxDist).toSet[Cell2D]
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
    val colorMapper: (Cell2D) => RGBColor = (c) => {
      if (pathSet.contains(c)) distMap.colorMapper(c)
      else RGBColor.fromAwt(java.awt.Color.GRAY)
    }
    image = ImageCreator.create(maze, cellSize, colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"LongestPath$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.DeadEndTest") {
    val algorithms = List[MazeGenerator](
      AldousBroderMaze, BinaryTreeMaze, HuntAndKillMaze,
      SidewinderMaze, RecurBackTrackMaze, WilsonMaze)
    var averages: Map[MazeGenerator, Int] = Map()
    val size = 20
    val grid = new GridEx(size,size)
    val tries = 100
    for (algo <- algorithms) {
      val counts = mutable.Seq[Int]() ++ (for (_ <- 0 until tries) yield 0)
      for (i <- 0 until tries) {
        val maze = algo.generate(rand, grid.asInstanceOf[algo.T])
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

  test("MaskedGridImageTest") {
    val mask =
      """X........X
        |....XX....
        |...XXXX...
        |....XX....
        |X........X
        |X........X
        |....XX....
        |...XXXX...
        |....XX....
        |X........X""".stripMargin
    val grid = MaskedGrid.from(mask)
    val maze = RecurBackTrackMaze.generate(rand, grid)
    var image = ImageCreator.create(maze, 32, Some(5))
    var f = FileHelper.saveToFile(image, PngWriter.MaxCompression,
      "MaskedGrid.png", "images")
    assert(f.isSuccess)
    val middle = grid(1, 1)
    val distMap = DistanceEx.createMax(maze, middle).get
    image = ImageCreator.create(distMap, 32, Some(5))
    f = FileHelper.saveToFile(image, PngWriter.MaxCompression,
      "MaskedGrid_DistMap.png", "images")
    assert(f.isSuccess)
    val (farCell, _) = distMap.max
    val path = distMap.pathTo(farCell).get
    pathCheck(path, distMap, distMap.root, farCell)
    val content = (cell: Cell2DCart) => {
      if (distMap.contains(cell)) distMap(cell).toString else "-"
    }
    print(maze.dump(content)); println()
    println(s"grid size=${grid.size}")
  }

  test("PolarMazeTest") {
    val polarGrid = PolarGrid(20)
    val maze = RecurBackTrackMaze.generate(rand, polarGrid)
    var image = ImageCreator.create(maze, 32, Some(5))
    var f = FileHelper.saveToFile(image, writer, s"PolarMaze$ext", dir)
    assert(f.isSuccess)
  }
}
