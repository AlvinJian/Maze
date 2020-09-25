package maze

import scala.annotation.tailrec

case class PolarGrid(radius: Int) extends PlainGrid[Position2D] {
  val (data: Seq[Position2D], offsets: Vector[Int]) = {
    val rowHeight: Double = 1.0
    @tailrec
    def buildData(r: Int, tmpData: Vector[Position2D],
                  offsets: Vector[Int], prevCount: Int): (Vector[Position2D], Vector[Int]) = {
      if (r == 0) {
        buildData(r+1, tmpData :+ Position2D(0,0), offsets :+ 0, 1)
      } else if (r < this.radius) {
        val radius = r.toDouble
        val circum = 2.0 * math.Pi * radius
        val estCellWidth = circum / prevCount.toDouble
        val ratio = (estCellWidth / rowHeight).round.toInt
        val newCellCount = prevCount * ratio
        val newCells = for (col <- 0 until newCellCount) yield Position2D(r, col)
        buildData(r+1, tmpData ++ Vector.from(newCells), offsets :+ tmpData.size, newCellCount)
      } else (tmpData, offsets :+ tmpData.size)
    }
    buildData(0, Vector[Position2D](), Vector[Int](), 0)
  }

  override def at(r: Int, c: Int): Option[Position2D] = {
    if (!isValid(Position2D(r,c))) return None
    val offset = this.offsets(r)
    val colCount = countAt(r)
    @tailrec
    def calcIndex(num: Int): Int = {
      if (num >= 0) num % colCount
      else calcIndex(num + colCount)
    }
    val index = calcIndex(c)
    Some(data(offset + index))
  }

  override def isValid(pos: Position2D): Boolean =  pos.row >= 0 && pos.row < radius

  override def iterator: Iterator[Position2D] = data.iterator

  def countAt(r: Int): Int = this.offsets(r+1) - this.offsets(r)
}
