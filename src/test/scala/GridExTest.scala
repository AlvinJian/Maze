import grid.GridEx
import org.scalatest.FunSuite

class GridExTest extends FunSuite {
  test("GridExTest") {
    val grid = new GridEx(2,2)
    val cell0 = grid(0, 0)
    val cell1 = grid(0, 1)
    val cell2 = grid(1, 0)
    val cell3 = grid(1, 1)

    assert(cell0.row == 0 && cell0.col == 0)
    assert(cell1.row == 0 && cell1.col == 1)
    assert(cell2.row == 1 && cell2.col == 0)
    assert(cell3.row == 1 && cell3.col == 1)

    for (cell <- grid) {
      val r = cell.row
      val c = cell.col
      assert(cell.north === {
        val other = if (r-1 >= 0) Some(grid(r-1, c)) else None
        other
      })
      assert(cell.south === {
        val other = if (r+1 < grid.row) Some(grid(r+1, c)) else None
        other
      })
      assert(cell.east === {
        val other = if (c+1 < grid.col) Some(grid(r, c+1)) else None
        other
      })
      assert(cell.west === {
        val other = if (c-1 >= 0) Some(grid(r, c-1)) else None
        other
      })
    }
  }
}
