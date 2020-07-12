import java.awt.Color

import algorithm.{AldousBroderMaze, BinaryTreeMaze, DistanceEx, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.nio.PngWriter
import grid.{Cell2D, Cell2DCart, Graph, GraphEx, GridEx, HexGrid, MaskedGrid, PolarGrid, TriangleGrid, WeaveGrid}
import org.scalatest.FunSuite
import utils.{FileHelper, ImageUtilsEx}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

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
    @tailrec
    def loopCheck(i: Int): Unit = {
      if (i < path.size-1) {
        assert(graph.isLinked(path(i), path(i+1)))
        assert(distMap(path(i))+1 == distMap(path(i+1)))
        loopCheck(i+1)
      }
    }
    loopCheck(0)
  }

  test("Algorithm.BinaryTreeEx") {
    val grid = new GridEx(20,20)
    val maze = BinaryTreeMaze.generate(rand, grid)
//    print(maze)
    val start = grid(0, 0)
    val distMap = DistanceEx.from(maze, start).get
    val end = grid((grid.rows-1)/2, (grid.cols-1)/2)
    val path = distMap.pathTo(end)
    assert(path.nonEmpty)
    pathCheck(path, distMap, start, end)
    val img = ImageUtilsEx.creationFunction(maze)(32, Some(5))
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
//    print(maze/*.dump(content)*/); println()
    val goal = grid((grid.rows-1)/2, (grid.cols-1)/2)
    val path = distMap.pathTo(goal)
    assert(path.nonEmpty)
    pathCheck(path, distMap, start, goal)
    val img = ImageUtilsEx.creationFunction(maze)(cellSize, padding)
    val file = FileHelper.saveToFile(img, writer, s"Sidewinder$ext", "images")
    assert(file.isSuccess)
  }

  test("Algorithm.AldousBroder") {
    val grid = new GridEx(20, 20)
    val maze = AldousBroderMaze.generate(rand, grid)
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = imgFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"AldousBroder$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid((grid.rows-1)/2, (grid.cols-1)/2)).get
    image = imgFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"AldousBroder_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.Wilson") {
    val grid = new GridEx(20, 20)
    val maze = WilsonMaze.generate(rand, grid)
    var image = ImageUtilsEx.creationFunction(maze)(cellSize, padding)
    var f = FileHelper.saveToFile(image, writer, s"Wilson$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.HuntAndKill") {
    val grid = new GridEx(20, 20)
    val maze = HuntAndKillMaze.generate(rand, grid)
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = imgFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"HuntAndKill$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = imgFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"HuntAndKill_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.RecursiveBackTrack") {
    val grid = new GridEx(20, 20)
    val maze = RecurBackTrackMaze.generate(rand, grid)
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = imgFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(9,9)).get
    image = imgFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"RecursiveBackTrack_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("Algorithm.LongestPath") {
    val grid = new GridEx(20,20)
    val maze = RecurBackTrackMaze.generate(rand, grid)
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = imgFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE), padding)
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
    image = imgFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"LongestPath_MaxDistMap$ext", dir)
    assert(f.isSuccess)
    val path = distMap.pathTo(farCell)
    pathCheck(path, distMap, distMap.root, farCell)
    val pathSet = path.toSet
//    println(maze.dump((c) => {
//      if (pathSet.contains(c)) distMap(c).toString else ""
//    }))
    val colorMapper: (Cell2D) => RGBColor = (c) => {
      if (pathSet.contains(c)) distMap.colorMapper(c)
      else RGBColor.fromAwt(java.awt.Color.GRAY)
    }
    image = imgFunc(cellSize, colorMapper, padding)
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
    println("average dead ends statistics:")
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
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = imgFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, PngWriter.MaxCompression,
      "MaskedGrid.png", "images")
    assert(f.isSuccess)
    val middle = grid(2, 0)
    val distMap = DistanceEx.createMax(maze, middle).get
    image = imgFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, PngWriter.MaxCompression,
      "MaskedGrid_DistMap.png", "images")
    assert(f.isSuccess)
    val (farCell, _) = distMap.max
    val path = distMap.pathTo(farCell)
    pathCheck(path, distMap, distMap.root, farCell)
    val content = (cell: Cell2DCart) => {
      if (distMap.contains(cell)) distMap(cell).toString else "-"
    }
//    print(maze.dump(content)); println()
    println(s"grid size=${grid.size}")
  }

  test("PolarMazeTest") {
    val polarGrid = PolarGrid(20)
    val maze = HuntAndKillMaze.generate(rand, polarGrid)
    val colorImageFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = colorImageFunc(cellSize, _ => RGBColor.fromAwt(java.awt.Color.WHITE) ,padding)
    var f = FileHelper.saveToFile(image, writer, s"PolarMaze$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, polarGrid(0, 0)).get
    image = colorImageFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"PolarMaze_DistMap$ext", dir)
    assert(f.isSuccess)

    val (farCell, _) = distMap.max
    val path = distMap.pathTo(farCell)
    assert(path.nonEmpty)
    pathCheck(path, distMap, distMap.root, farCell)
    val pathSet = path.toSet
    def colorMapper: (Cell2D)=>RGBColor = (cell) => {
      if (pathSet.contains(cell)) distMap.colorMapper(cell)
      else RGBColor.fromAwt(Color.DARK_GRAY)
    }
    image = colorImageFunc(cellSize, colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"PolarMaze_Path$ext", dir)
    assert(f.isSuccess)
  }

  test("HexMazeTest") {
    val grid = HexGrid(10, 10)
    val maze = HuntAndKillMaze.generate(rand, grid)
    val colorImageFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = colorImageFunc(cellSize, _=>RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"HexMaze$ext", dir)
    assert(f.isSuccess)
    val distMap: DistanceEx = DistanceEx.createMax(maze, grid(5,5)).get
    image = colorImageFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"HexMaze_DistMap$ext", dir)
    assert(f.isSuccess)
  }

  test("TriangleMazeTest") {
    val grid = TriangleGrid(15, 20)
    val maze = HuntAndKillMaze.generate(rand, grid)
    val colorImageFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = colorImageFunc(cellSize,_=>RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"TriangleMaze$ext", dir)
    assert(f.isSuccess)
    val distMap: DistanceEx = DistanceEx.createMax(maze, grid(5,5)).get
    image = colorImageFunc(cellSize, distMap.colorMapper, padding)
    f = FileHelper.saveToFile(image, writer, s"TriangleMaze_DistMap$ext", dir)
    assert(f.isSuccess)

    val (farCell, _) = distMap.max
    val path = distMap.pathTo(farCell)
    pathCheck(path, distMap, distMap.root, farCell)
    def colorPath(cell: Cell2D): RGBColor = {
      if (path.contains(cell)) distMap.colorMapper(cell)
      else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
    }
    image = colorImageFunc(cellSize, colorPath, padding)
    f = FileHelper.saveToFile(image, writer, s"TriangleMaze_Path$ext", dir)
    assert(f.isSuccess)
  }

  test("BraidTest") {
    val grid = PolarGrid(15)
    var maze = HuntAndKillMaze.generate(rand, grid)
    val deadEndCount = maze.deadEnds.size
    maze = Graph.braid(rand, maze, 0.85)
    val deadEndCount2 = maze.deadEnds.size
    assert(deadEndCount2 < deadEndCount)
    println(s"before braid: $deadEndCount; after braid: $deadEndCount2")
    val colorImageFunc = ImageUtilsEx.creationFunctionWithColor(maze)
    var image = colorImageFunc(cellSize,_=>RGBColor.fromAwt(java.awt.Color.WHITE), padding)
    var f = FileHelper.saveToFile(image, writer, s"BraidMaze$ext", dir)
    assert(f.isSuccess)
    val distMap = DistanceEx.createMax(maze, grid(0,0)).get
    val (farCell, _) = distMap.max
    val path = distMap.pathTo(farCell)
    pathCheck(path, distMap, distMap.root, farCell)
    def colorPath(cell: Cell2D): RGBColor = {
      if (path.contains(cell)) distMap.colorMapper(cell)
      else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
    }
    image = colorImageFunc(cellSize, colorPath, padding)
    f = FileHelper.saveToFile(image, writer, s"BraidMaze_Path$ext", dir)
    assert(f.isSuccess)
  }

  test("InsetMazeImageTest") {
    val grid = GridEx(20, 20)
    val maze = RecurBackTrackMaze.generate(rand, grid)
    var func = ImageUtilsEx.creationFunctionEx(maze)
    var img = func(cellSize, padding.get, 2)
    var file = FileHelper.saveToFile(img, writer, s"InsetMaze$ext", dir)
    assert(file.isSuccess)

    val distanceEx = DistanceEx.createMax(maze, grid(10,10)).get
    val colorFunc = ImageUtilsEx.creationColoredFunctionEx(maze)
    img = colorFunc(cellSize, 2, distanceEx.colorMapper, 2)
    file = FileHelper.saveToFile(img, writer, s"InsetMaze_Color$ext", dir)
    assert(file.isSuccess)
  }

  test("WeaveMazeImageTest") {
    val grid = WeaveGrid(20, 20)
    val maze = RecurBackTrackMaze.generate(rand, grid)
    var func = ImageUtilsEx.creationFunctionEx(maze)
    var img = func(cellSize, 2, padding.get)
    var file = FileHelper.saveToFile(img, writer, s"WeaveMaze$ext", dir)
    assert(file.isSuccess)

    val distanceEx = DistanceEx.createMax(maze, grid(10,10)).get
    val colorFunc = ImageUtilsEx.creationColoredFunctionEx(maze)
    img = colorFunc(cellSize, 2, distanceEx.colorMapper, padding.get)
    file = FileHelper.saveToFile(img, writer, s"WeaveMaze_Color$ext", dir)
    assert(file.isSuccess)
    val farCell = distanceEx.max._1
    val path = distanceEx.pathTo(farCell)
    assert(path.nonEmpty)
    pathCheck(path, distanceEx, distanceEx.root, farCell)
    img = colorFunc(cellSize, 8, (c: Cell2D)=> {
      if (path.contains(c)) distanceEx.colorMapper(c)
      else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
    }, padding.get)
    file = FileHelper.saveToFile(img, writer, s"WeaveMaze_Path$ext", dir)
    assert(file.isSuccess)
  }
}
