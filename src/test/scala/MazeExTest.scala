import maze.{Cell2D, Cell2DRect, Maze, Position2D, RectGrid, RectMaze, RectMazeInfo}
import org.scalatest.FunSuite

class MazeExTest extends FunSuite {
  test("RectMazeStructTest") {
    var rectMaze: Maze[Cell2DRect] = RectMaze(5,5)
    val info = rectMaze.info
    info match {
      case rectMazeInfo: RectMazeInfo => {
        assert(rectMazeInfo.grid.rows == 5 && rectMazeInfo.grid.cols == 5)
        assert(rectMazeInfo.name == "RectMaze")
      }
      case _ => assert(false)
    }
    for (cell <- rectMaze) {
      val r = cell.pos.row
      val c = cell.pos.col
      assert(cell.north === rectMaze.at(r - 1, c))
      assert(cell.south === rectMaze.at(r + 1, c))
      assert(cell.east === rectMaze.at(r, c + 1))
      assert(cell.west === rectMaze.at(r, c - 1))
    }
    val p1 = Position2D(0,0)
    val p2 = Position2D(1,1)
    rectMaze = rectMaze.link(p1, p2).get
    assert(rectMaze.at(p1).get.linked.size == 1)
    assert(rectMaze.at(p1).get.linked.head.pos === p2)
    assert(rectMaze.at(p2).get.linked.size == 1)
    assert(rectMaze.at(p2).get.linked.head.pos === p1)
    assert(rectMaze.at(3,3).get.linked.isEmpty)
  }
}
