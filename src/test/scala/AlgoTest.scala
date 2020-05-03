import algorithm.{BinaryTree, CellLink, Sidewinder}
import grid.Grid
import org.scalatest.FunSuite

class AlgoTest extends FunSuite {
  val grid = new Grid(8, 10)

  test("Algorithm.BinaryTree") {

    val solver = new BinaryTree
    val result = solver.solve(grid)
    val s = CellLink.graphToString(result)
    print(s)
  }

  test("Algorithm.Sidewinder") {
    val solver = new Sidewinder
    val result = solver.solve(grid)
    val s = CellLink.graphToString(result)
    print(s)
  }
}
