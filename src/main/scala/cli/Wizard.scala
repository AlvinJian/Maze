package cli

import java.io.File

import algorithm.{AldousBroderMaze, BinaryTreeMaze, DistanceEx, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import com.sksamuel.scrimage.ImmutableImage
import grid.{Cell2D, CellContainer, GraphEx, GridEx, HexGrid, MaskedGrid, PolarGrid, TriangleGrid}
import com.sksamuel.scrimage.color.RGBColor
import utils.ImageUtilsEx

import scala.swing.FileChooser
import scala.util.{Failure, Random, Success, Try}


object Wizard {
  private val rand: Random = new Random(System.currentTimeMillis())

  @scala.annotation.tailrec
  def numberFromStdIn(optMin: Option[Int]=None, optMax: Option[Int]=None): Int = {
    val iTrial: Try[Int] = Try(scala.io.StdIn.readInt())
    val min = optMin match {
      case Some(value) => value
      case None => Int.MinValue
    }
    val max = optMax match {
      case Some(value) => value
      case None => Int.MaxValue
    }
    iTrial match {
      case Success(value) => {
        if (value >= min && value <= max) value
        else {
          println(s"out of bound: [$min, $max]")
          numberFromStdIn(optMin, optMax)
        }
      }
      case Failure(exception) => {
        println("invalid input. again...")
        numberFromStdIn(optMin, optMax)
      }
    }
  }

  @scala.annotation.tailrec
  def promptForAnswer(msg: String): String = {
    print(msg)
    val ans = scala.io.StdIn.readLine().trim
    if (ans.length > 0) ans
    else promptForAnswer(msg)
  }

  def setupWorkspace: Option[File] = {
    val jc: FileChooser = new FileChooser()
    val title: String = "select a directory as a workspace:"
    println(title)
    jc.title = title
    jc.multiSelectionEnabled = false
    jc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
    val ret = jc.showOpenDialog(null)
    if (ret == FileChooser.Result.Approve) Some(jc.selectedFile)
    else None
  }

  def setupGrid: (CellContainer[Cell2D], Int) = {
    val gridTypes = Vector(GridEx.getClass.getSimpleName, PolarGrid.getClass.getSimpleName,
      HexGrid.getClass.getSimpleName, TriangleGrid.getClass.getSimpleName)
    val rectId = gridTypes.indexOf(GridEx.getClass.getSimpleName)
    val hexId = gridTypes.indexOf(HexGrid.getClass.getSimpleName)
    val triId = gridTypes.indexOf(TriangleGrid.getClass.getSimpleName)
    val polarId = gridTypes.indexOf(PolarGrid.getClass.getSimpleName)
    for (i <- gridTypes.indices) {
      println(s"${i}: ${gridTypes(i)}")
    }
    print(s"pick one (${gridTypes.indices.head}-${gridTypes.indices.last}): ")
    val choice = numberFromStdIn(Some(0), Some(gridTypes.indices.last))
    println(s"you choose ${gridTypes(choice)}")
    val grid: CellContainer[Cell2D] = choice match {
      case i if (i == rectId || i == triId || i == hexId) => {
        print("enter column count: ")
        val cols = numberFromStdIn(Some(1))
        print("enter row count: ")
        val rows = numberFromStdIn(Some(1))
        i match {
          case j if j == rectId => GridEx(rows, cols)
          case k if k == triId => TriangleGrid(rows, cols)
          case l if l == hexId => HexGrid(rows, cols)
        }
      }
      case j if j == polarId => {
        print("enter cell count along radius: ")
        val rows = numberFromStdIn(Some(1))
        PolarGrid(rows)
      }
      case _ => ???
    }
    print("enter cell size: ")
    val cellSize = numberFromStdIn(Some(1))
    (grid, cellSize)
  }

  def generateMaze(grid: CellContainer[Cell2D]): GraphEx = {
    val common = List[MazeGenerator](AldousBroderMaze, HuntAndKillMaze, RecurBackTrackMaze, WilsonMaze)
    val algos = grid match {
      case GridEx(_, _) => common ++ List(SidewinderMaze, BinaryTreeMaze)
      case MaskedGrid(_, _, _) => common ++ List(SidewinderMaze, BinaryTreeMaze)
      case TriangleGrid(_, _) => common ++ List(SidewinderMaze, BinaryTreeMaze)
      case _ => common
    }
    for (i <- algos.indices) {
      println(s"${i}: ${algos(i).getClass.getSimpleName}")
    }
    print(s"pick an algorithm (${algos.indices.head}-${algos.indices.last}): ")
    val choice = numberFromStdIn(Some(0), Some(algos.indices.last))
    println(s"you choose ${algos(choice).getClass.getSimpleName}")
    val gen: MazeGenerator = algos(choice)
    gen.generate(rand, grid.asInstanceOf[gen.T])
  }

  @scala.annotation.tailrec
  def setupSeedCell(grid: CellContainer[Cell2D]): Cell2D = {
    println("setup the seed to generate the longest path")
    print("enter r: ")
    val r = numberFromStdIn(Some(0))
    print("enter c: ")
    val c = numberFromStdIn(Some(0))
    if (grid.isValid(r, c)) {
      println(s"you pick ($r, $c)")
      grid(r, c)
    }
    else {
      println("invalid position for the grid. again...")
      setupSeedCell(grid)
    }
  }

  @scala.annotation.tailrec
  def computeDistMap(maze: GraphEx): DistanceEx = {
    println("Compute the longest path")
    val seed = setupSeedCell(maze.grid)
    val opt = DistanceEx.createMax(maze, seed)
    opt match {
      case Some(value) => {
        println("done!")
        value
      }
      case None => {
        println("Something wrong with that seed. again...")
        computeDistMap(maze)
      }
    }
  }

  def createImageWithStartAndEnd(distMap: DistanceEx, cellSize: Int, padding: Option[Int]): ImmutableImage = {
    val start = distMap.root
    val (end, _) = distMap.max
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(distMap.graph)
    def colorCell(cell: Cell2D) = {
      if (cell == start || cell == end) distMap.colorMapper(cell)
      else RGBColor.fromAwt(java.awt.Color.WHITE)
    }
    imgFunc(cellSize, colorCell, padding)
  }

  def createImageWithPath(distMap: DistanceEx, cellSize: Int, padding: Option[Int]): ImmutableImage = {
    val path = distMap.pathTo(distMap.max._1)
    val cellSet = path.toSet
    val imgFunc = ImageUtilsEx.creationFunctionWithColor(distMap.graph)
    def colorCell(cell: Cell2D): RGBColor = {
      if (cellSet.contains(cell)) distMap.colorMapper(cell)
      else RGBColor.fromAwt(java.awt.Color.DARK_GRAY)
    }
    imgFunc(cellSize, colorCell, padding)
  }
}
