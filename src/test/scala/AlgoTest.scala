import algorithm.{BinaryTree, CellLink}
import grid.Grid
import org.scalatest.FunSuite

class AlgoTest extends FunSuite {
  test("Algorithm.BinaryTree") {
    val grid = new Grid(8, 10)
    val solver = new BinaryTree
    val result = solver.solve(grid)
    val s = CellLink.graphToString(result, grid)
    print(s)
  }
}
