package grid
import scala.util.Random

class MaskedGrid(override val row: Int, override val col: Int,
                 blacklist: Set[(Int,Int)]) extends GridEx(row, col) {
  private val maskedCells: Set[CellEx] = data.filter((c)=>{
    val pos = (c.row, c.col)
    blacklist.contains(pos)
  }).toSet

  override def isValid(cell: CellEx): Boolean = !maskedCells.contains(cell) && super.isValid(cell)

  def isValid(r: Int, c: Int): Boolean = this.isValid(this(r,c))

  // if the adjacent cell is invalid, return None
  override def adjacencyOf(cell: CellEx, direction: Direction): Option[CellEx] = {
    var ret = super.adjacencyOf(cell, direction)
    if (ret.isDefined && maskedCells.contains(ret.get)) ret = None
    ret
  }

  override def randomCell(r: Random): CellEx = {
    var cell = super.randomCell(r)
    while (maskedCells.contains(cell)) cell = super.randomCell(r)
    cell
  }

  // TODO implement new Iterator skipping masked cells
  override def iterator: Iterator[CellEx] = new Iterator[CellEx] {
    override def hasNext: Boolean = ???

    override def next(): CellEx = ???
  }
}
