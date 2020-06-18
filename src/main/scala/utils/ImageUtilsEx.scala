package utils

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import grid.{Cell2D, GraphEx, GridEx, HexGrid, MaskedGrid, PolarGrid}

trait MazeImageCreator {
  val graph: GraphEx
  val cellSize: Int
  def baseImage: ImmutableImage
  def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage
  def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage
}

object MazeImageCreator {
  def make(graph: GraphEx, cellSize: Int): MazeImageCreator =
    graph.grid match {
      case hexGrid: HexGrid => new HexMazeImageCreator(graph, cellSize)
      case polarGrid: PolarGrid => new PolarMazeImageCreator(graph, cellSize)
      case grid: GridEx => new CartesianMazeImageCreator(graph, cellSize)
      case maskedGrid: MaskedGrid => new CartesianMazeImageCreator(graph, cellSize)
      case _ => ???
    }
}

object ImageUtilsEx {
  def creationFunctionWithColor(graph: GraphEx): (Int, Cell2D=>RGBColor, Option[Int]) => ImmutableImage = {
    val ret: (Int, Cell2D=>RGBColor, Option[Int]) => ImmutableImage = (size, f, padding)=> {
      val start = (cellSize: Int) => MazeImageCreator.make(graph, cellSize)
      val pipeline = start.andThen{
        creator => (creator, creator.baseImage)
      }.andThen{
        tup => {
          val creator: MazeImageCreator = tup._1
          val baseImage: ImmutableImage = tup._2
          (creator, creator.drawColoredCells(baseImage, f))
        }
      }.andThen{
        tup => {
          val creator: MazeImageCreator = tup._1
          val baseImage: ImmutableImage = tup._2
          creator.drawMazeWalls(baseImage)
        }
      }
      val img = pipeline(size)
      if (padding.isDefined) img.pad(padding.get, java.awt.Color.GRAY) else img
    }
    ret
  }

  def creationFunction(graph: GraphEx): (Int, Option[Int])=>ImmutableImage = {
    val start = (cellSize: Int) => {
      MazeImageCreator.make(graph, cellSize)
    }
    val baseFunc = start.andThen{
      (creator: MazeImageCreator) => (creator, creator.baseImage)
    }.andThen{
      tup => {
        val creator: MazeImageCreator = tup._1
        val baseImage: ImmutableImage = tup._2
        creator.drawMazeWalls(baseImage)
      }
    }
    val ret = (cellSize: Int, padding: Option[Int]) => {
      val img = baseFunc(cellSize)
      if (padding.isDefined) img.pad(padding.get, java.awt.Color.GRAY)
      else img
    }
    ret
  }
}
