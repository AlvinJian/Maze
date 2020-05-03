package algorithm

import grid.{Cell, Direction, Grid}
import grid.{NorthDir, WestDir, EastDir, SouthDir}

import scala.collection.mutable.Set

class CellLink(val cell: Cell) {
  private val _linked = Set[CellLink]()

  def link(c: CellLink, bidir: Boolean = true): Unit = {
    _linked.add(c)
    if (bidir) c.link(this, false)
  }

  def unlink(c: CellLink, bidir: Boolean = true): Unit = {
    _linked.remove(c)
    if (bidir) c.unlink(this, false)
  }

  def isLinked(c: CellLink): Boolean = _linked.contains(c)
}

object CellLink {
  def createFrom(grid: Grid): Vector[Vector[CellLink]] = Vector.from(
    for (r <- 0 until grid.row)
      yield {
        val aRow = for (c <- 0 until grid.col) yield new CellLink(grid(r,c))
        Vector.from(aRow)
      }
  )

  def graphToString(graph: Vector[Vector[CellLink]]): String = {
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
}
