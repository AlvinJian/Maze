package cli

import java.io.File

import algorithm.{AldousBroderMaze, BinaryTreeMaze, HuntAndKillMaze, MazeGenerator, RecurBackTrackMaze, SidewinderMaze, WilsonMaze}
import grid.{Cell2D, CellContainer, GraphEx, GridEx, HexGrid, MaskedGrid, PolarGrid}

import scala.swing.FileChooser
import scala.util.Random


object Wizard {
  private val rand: Random = new Random(System.currentTimeMillis())

  @scala.annotation.tailrec
  def numberFromStdIn(min: Int, max: Int): Int = {
    val i = try {
      scala.io.StdIn.readInt()
    } catch {
      case _: Throwable => Int.MinValue
    }
    if (i >= min && i <= max) i
    else {
      println(s"out of bound! [$min, $max] or invalid for a number")
      numberFromStdIn(min, max)
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
    val gridTypes = Vector("Rectangle Grid", "Circular Grid", "Hexagon Grid")
    for (i <- gridTypes.indices) {
      println(s"${i}: ${gridTypes(i)}")
    }
    print(s"pick one (${gridTypes.indices.head}-${gridTypes.indices.last}): ")
    val choice = numberFromStdIn(0, gridTypes.indices.last)
    println(s"you choose ${gridTypes(choice)}")
    val grid: CellContainer[Cell2D] = choice match {
      case i if (i == 0 || i == 2) => {
        print("enter column count: ")
        val cols = numberFromStdIn(1, Int.MaxValue)
        print("enter row count: ")
        val rows = numberFromStdIn(1, Int.MaxValue)
        if (i == 0) GridEx(rows, cols)
        else HexGrid(rows, cols)
      }
      case 1 => {
        print("enter cell count along radius: ")
        val rows = numberFromStdIn(1, Int.MaxValue)
        PolarGrid(rows)
      }
      case _ => ???
    }
    print("enter cell size: ")
    val cellSize = numberFromStdIn(1, 128)
    (grid, cellSize)
  }

  def generateMaze(grid: CellContainer[Cell2D]): GraphEx = {
    val common = List[MazeGenerator](AldousBroderMaze, HuntAndKillMaze,
      RecurBackTrackMaze, WilsonMaze)
    val algos = grid match {
      case GridEx(_, _) => common ++ List(SidewinderMaze, BinaryTreeMaze)
      case MaskedGrid(_, _, _) => common ++ List(SidewinderMaze, BinaryTreeMaze)
      case _ => common
    }
    for (i <- algos.indices) {
      println(s"${i}: ${algos(i).getClass.getSimpleName}")
    }
    print(s"pick an algorithm (${algos.indices.head}-${algos.indices.last}): ")
    val choice = numberFromStdIn(0, algos.indices.last)
    println(s"you choose ${algos(choice).getClass.getSimpleName}")
    val gen: MazeGenerator = algos(choice)
    gen.generate(rand, grid.asInstanceOf[gen.T])
  }
}
