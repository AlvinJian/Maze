import algorithm.CellLink
import grid.{Cell, Grid}
import org.scalatest.FunSuite
import org.scalatest.PrivateMethodTester._

@deprecated
class GridTest extends FunSuite {
  test("GridTest.cell") {
    val cell = new Cell(1,1)
    assert(cell.north === None)
    assert(cell.east === None)
    assert(cell.west === None)
    assert(cell.south === None)

    val ncell = new Cell(0, 1)
    val assignNorth = PrivateMethod[Cell](Symbol("north_$eq"))
    cell.invokePrivate(assignNorth(ncell))
    assert(cell.north === Some(ncell))
    assert(ncell.south === Some(cell))
    val wcell = new Cell(1, 0)
    val assignWest = PrivateMethod[Cell](Symbol("west_$eq"))
    cell.invokePrivate(assignWest(wcell))
    assert(cell.west === Some(wcell))
    assert(wcell.east === Some(cell))
  }

  test("GridTest.grid") {
    val rowMax = 3
    val colMax = 4
    val grid = new Grid(rowMax, colMax)
    var count = 0
    for (c <- grid) {
      assert(c.row * grid.col + c.col === count)
      count += 1
    }
    assert(count === rowMax * colMax)
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

  test("GridTest.link") {
    val graph = CellLink.createFrom(new Grid(4,4))
    graph(0)(0).link(graph(3)(3))
    graph(1)(2).link(graph(2)(1))

    assert(graph(3)(3).isLinked(graph(0)(0)))
    assert(graph(2)(1).isLinked(graph(1)(2)))
    assert(graph(1)(1).isLinked(graph(2)(2)) === false)
  }
}
