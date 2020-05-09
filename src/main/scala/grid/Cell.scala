package grid

import scala.collection.mutable.Map
import scala.collection.mutable.Set

@deprecated
class Cell(val row: Int, val col: Int) {
  private var _neighbors = Map[Direction, Cell]()

  def north: Option[Cell] = _neighbors.get(NorthDir)
  private[grid] def north_=(c: Cell): Unit = {
    val _ = _neighbors.put(NorthDir, c)
    val _ = c._neighbors.put(Direction.reverse(NorthDir), this)
  }

  def south: Option[Cell] = _neighbors.get(SouthDir)
  private[grid] def south_=(c: Cell): Unit = {
    val _ = _neighbors.put(SouthDir, c)
    val _ = c._neighbors.put(Direction.reverse(SouthDir), this)
  }

  def east: Option[Cell] = _neighbors.get(EastDir)
  private[grid] def east_=(c: Cell): Unit = {
    val _ = _neighbors.put(EastDir, c)
    val _ = c._neighbors.put(Direction.reverse(EastDir), this)
  }

  def west: Option[Cell] = _neighbors.get(WestDir)
  private[grid] def west_=(c: Cell): Unit = {
    val _ = _neighbors.put(WestDir, c)
    val _ = c._neighbors.put(Direction.reverse(WestDir), this)
  }

  def neighbors: List[Cell] = List(this.north, this.south, this.east, this.west).flatten

  override def toString: String = s"Cell(${row},${col})"

//  override def equals(obj: Any): Boolean = obj match {
//    case c: Cell => c.col == this.col && c.row == this.row
//    case _ => false
//  }
//
//  override def hashCode(): Int = s"$row,$col".hashCode
}
