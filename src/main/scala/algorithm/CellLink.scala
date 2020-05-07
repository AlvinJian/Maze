package algorithm

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.color.{Color, RGBColor}
import grid.{Cell, Grid}
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}

import scala.collection.mutable.Set

// immutable interface
trait CellLinkReader {
  def cell: Cell
  def isLinked(link: CellLinkReader): Boolean
  def linked: IndexedSeq[CellLinkReader]
}

class LinkGraph {

}

// mutable class
class CellLink(val cell: Cell) extends CellLinkReader {
  private val _linked = Set[CellLinkReader]()

  def link(c: CellLink, bidir: Boolean = true): Unit = {
    _linked.add(c)
    if (bidir) c.link(this, false)
  }

  def link (c: CellLink, bidir: Boolean = true): CellLink = {
    val newLink = new CellLink(c.cell)
    newLink._linked.addAll(this._linked).add(c)

    newLink
  }

  def unlink(c: CellLink, bidir: Boolean = true): Unit = {
    _linked.remove(c)
    if (bidir) c.unlink(this, false)
  }

  override def isLinked(c: CellLinkReader): Boolean = _linked.contains(c)

  override def linked: IndexedSeq[CellLinkReader] = _linked.toIndexedSeq
}

object CellLink {
  def createFrom(grid: Grid): Vector[Vector[CellLink]] = Vector.from(
    for (r <- 0 until grid.row)
      yield {
        val aRow = for (c <- 0 until grid.col) yield new CellLink(grid(r,c))
        Vector.from(aRow)
      }
  )

  def graphToString(graph: Vector[Vector[CellLinkReader]]): String = {
    val colRange = graph(0).indices
    val rowRange = graph.indices
    val _out = new StringBuilder("+")
    _out.append(new String(colRange.flatMap(_ => "---+").toArray) + "\n")
    for (r <- rowRange) {
      val topSb = new StringBuilder("|")
      val bottomSb = new StringBuilder("+")
      for (c <- colRange) {
        val link = graph(r)(c)
        val cell = link.cell
        val bottomWall = cell.south match {
          case Some(south) => {
            if (link.isLinked(graph(south.row)(south.col))) "   " else "---"
          }
          case _ => "---"
        }
        bottomSb.append(bottomWall).append("+")
        val eastWall = cell.east match {
          case Some(east) => {
            if (link.isLinked(graph(east.row)(east.col))) " " else "|"
          }
          case _ => "|"
        }
        topSb.append("   ").append(eastWall)
      }
      _out.append(topSb.append('\n')).append(bottomSb.append('\n'))
    }
    _out.toString()
  }

  def graphToImage(graph: Vector[Vector[CellLinkReader]], cellSize: Int = 10): ImmutableImage = {
    val imgWidth = cellSize * graph(0).size
    val imgHeight = cellSize * graph.size
    val mutableImage = new MutableImage(new BufferedImage(imgWidth+1, imgHeight+1, BufferedImage.TYPE_INT_RGB))
    val bgColor = new RGBColor(255, 255, 255)
    val wallColor = new RGBColor(0, 0, 0)
    mutableImage.fillInPlace(bgColor.awt())

    val drawLine = (x1: Int, y1: Int, x2: Int, y2: Int, c: Color) => {
      for {
        i <- x1 to x2
        j <- y1 to y2
      } {
        mutableImage.setColor(i, j, c)
      }
    }
    val colRange = graph(0).indices
    val rowRange = graph.indices
    for (r <- rowRange) {
      for (c <- colRange) {
        val link = graph(r)(c)
        val cell = link.cell
        val x1 = cell.col * cellSize
        val y1 = cell.row * cellSize
        val x2 = (cell.col+1) * cellSize
        val y2 = (cell.row+1) * cellSize

        cell.north match {
          case None => drawLine(x1, y1, x2, y1, wallColor)
          case _ => { }
        }
        cell.west match {
          case None => drawLine(x1, y1, x1, y2, wallColor)
          case _ => { }
        }
        val shouldDrawEast = cell.east match {
          case Some(ecell) => {
            val elink = graph(ecell.row)(ecell.col)
            !link.isLinked(elink)
          }
          case _ => true
        }
        if (shouldDrawEast) drawLine(x2, y1, x2, y2, wallColor)
        val shouldDrawSouth = cell.south match {
          case Some(scell) => {
            val slink = graph(scell.row)(scell.col)
            !link.isLinked(slink)
          }
          case _ => true
        }
        if (shouldDrawSouth) drawLine(x1, y2, x2, y2, wallColor)
      }
    }
    mutableImage.toImmutableImage.pad(5, bgColor.awt())
  }
}
