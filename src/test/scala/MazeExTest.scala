import maze.{Cell2D, Cell2DPolar, Cell2DRect, Cell2DTriangle, Maze, PolarGrid, PolarMaze, PolarMazeInfo, Position2D, RectGrid, RectMaze, RectMazeInfo, TriangleMaze}
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

  test("PolarMazeStructTest") {
    val radius = 10
    var polarMaze: Maze[Cell2DPolar] = PolarMaze(radius)
    val info = polarMaze.info
    val grid: PolarGrid = info match {
      case PolarMazeInfo(grid, maze) => {
        assert(grid.radius == radius)
        grid
      }
      case _ => {
        assert(false)
        PolarGrid(0) // whatever...
      }
    }
    val total = { 0.until(radius) }.map(r => grid.countAt(r)).sum
    assert(total == polarMaze.size)
    for (cell <- polarMaze) {
      val (row: Int, col: Int) = (cell.pos.row, cell.pos.col)
      println(s"zzz $row, $col")
      if (row == 0) {
        assert(cell.cw == cell)
        assert(cell.ccw == cell)
        assert(cell.outward.size == 6)
        assert(cell.inward.isEmpty)
      } else {
        assert(cell.cw == polarMaze.at(row, col-1).get)
        assert(cell.ccw == polarMaze.at(row, col+1).get)
        if (row < grid.radius-1) {
          val count = grid.countAt(row)
          val outerCount = grid.countAt(row+1)
          assert(cell.outward.size == { if (outerCount > count) 2 else 1 })
        } else {
          assert(cell.outward.isEmpty)
        }
      }
    }
  }

  test("TriangleMazeStructTest") {
    val triMaze = TriangleMaze(10, 10)
    val _ = triMaze.foldLeft[Option[Cell2DTriangle]](None)((optPrevCell, cell) => {
      optPrevCell match {
        case Some(prevCell) => {
          if (cell.pos.col > 0) {
            assert(prevCell.isUpright != cell.isUpright)
          }
        }
        case None =>
      }

      val r = cell.pos.row
      val c = cell.pos.col
      assert(cell.north == {
        if (cell.isUpright) None else triMaze.at(r - 1, c)
      })
      assert(cell.south == {
        if (cell.isUpright) triMaze.at(r + 1, c) else None
      })
      assert(cell.east == triMaze.at(r, c + 1))
      assert(cell.west == triMaze.at(r, c - 1))

      Some(cell)
    })
  }
}
