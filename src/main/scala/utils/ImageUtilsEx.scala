package utils

import algorithm.DistanceEx
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import grid.{Cell2D, GraphEx, HexGrid}

trait MazeImageCreator {
  val graph: GraphEx
  val cellSize: Int
  def baseImage: ImmutableImage
  def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage
  def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage
}

object ImageUtilsEx {
  // TODO create maze image creators for other kinds of grid
  def MakeImageCreator(graph: GraphEx, cellSize: Int): MazeImageCreator =
    graph.grid match {
      case hexGrid: HexGrid => new HexMazeImageCreator(graph, cellSize)
      case _ => ???
    }

  def MakeCreationFunction(graph: GraphEx): (Int, Option[Int])=>ImmutableImage = {
    val start = (cellSize: Int) => {
      MakeImageCreator(graph, cellSize)
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

  def MakeCreationFunction(distMap: DistanceEx): (Int, Option[Int])=>ImmutableImage = {
    val start = (cellSize: Int) => {
      MakeImageCreator(distMap.graph, cellSize)
    }
    val baseFunc = start.andThen{
      (creator: MazeImageCreator) => (creator, creator.baseImage)
    }.andThen{
      tup => {
        val creator: MazeImageCreator = tup._1
        val baseImage: ImmutableImage = tup._2
        (creator, creator.drawColoredCells(baseImage, distMap.colorMapper))
      }
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
