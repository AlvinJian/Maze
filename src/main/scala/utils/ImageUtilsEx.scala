package utils

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import grid.{Cell2D, Graph, GraphEx, RectGrid, HexGrid, MaskedGrid, PolarGrid, TriangleGrid, WeaveGrid}

trait MazeImageCreator {
  val graph: Graph
  val cellSize: Int
  def baseImage: ImmutableImage
  def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage
  def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage
}

object MazeImageCreator {
  def apply(graph: Graph, cellSize: Int): MazeImageCreator =
    graph.grid match {
      case hexGrid: HexGrid => new HexMazeImageCreator(graph, cellSize)
      case polarGrid: PolarGrid => new PolarMazeImageCreator(graph, cellSize)
      case grid: RectGrid => new CartesianMazeImageCreator(graph, cellSize)
      case maskedGrid: MaskedGrid => new CartesianMazeImageCreator(graph, cellSize)
      case triangleGrid: TriangleGrid => new TriangleMazeImageCreator(graph, cellSize)
      case _ => ???
    }

  def apply(graph: Graph, cellSize: Int, inSet: Int): MazeImageCreator = {
    if (inSet <= 0) apply(graph, cellSize)
    else {
      graph.grid match {
        case RectGrid(_, _) => new CarteMazeInsetImageCreator(graph, cellSize, inSet)
        case WeaveGrid(_, _) => new WeaveMazeImageCreator(graph, cellSize, inSet)
        case _ => ???
      }
    }
  }
}

object ImageUtilsEx {
  @deprecated
  def creationFunctionWithColor(graph: Graph): (Int, Cell2D=>RGBColor, Option[Int]) => ImmutableImage = {
    val ret: (Int, Cell2D=>RGBColor, Option[Int]) => ImmutableImage = (size, f, padding)=> {
      val start = (cellSize: Int) => MazeImageCreator(graph, cellSize)
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

  @deprecated
  def creationFunction(graph: Graph): (Int, Option[Int])=>ImmutableImage = {
    val start = (cellSize: Int) => {
      MazeImageCreator.apply(graph, cellSize)
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

  def creationFunctionEx(graph: Graph): (Int, Int, Int)=>ImmutableImage = {
    val f = (cellSize: Int, inSet: Int, padding: Int) => {
      val creator: MazeImageCreator = MazeImageCreator(graph, cellSize, inSet)
      val img = creator.drawMazeWalls(creator.baseImage)
      if (padding > 0) img.pad(padding, java.awt.Color.GRAY)
      else img
    }
    f
  }

  def creationColoredFunctionEx(graph: Graph): (Int, Int, Cell2D => RGBColor, Int)=>ImmutableImage = {
    val ret = (cellSize: Int, inSet: Int, f: Cell2D => RGBColor, padding: Int) => {
      val creator: MazeImageCreator = MazeImageCreator(graph, cellSize, inSet)
      var img = creator.drawColoredCells(creator.baseImage, f)
      img = creator.drawMazeWalls(img)
      if (padding > 0) img.pad(padding, java.awt.Color.GRAY)
      else img
    }
    ret
  }
}
