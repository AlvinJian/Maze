import maze.{Cell2DRect, Maze, RectMaze, RectMazeDimension}
import org.scalatest.FunSuite

class MazeExTest extends FunSuite {
  test("RectMazeStructTest") {
    var rectMaze: Maze[Cell2DRect] = new RectMaze(5, 5)
    val dim = rectMaze.dimension.asInstanceOf[RectMazeDimension]
    for (cell <- rectMaze) {
      val r = cell.pos.row
      val c = cell.pos.col
      cell.north match {
        case Some(northCell) => northCell.sameAs(rectMaze.at(r-1, c).get)
        case None => assert(r-1 < 0)
      }
      cell.south match {
        case Some(southCell) => southCell.sameAs(rectMaze.at(r+1, c).get)
        case None => assert(r+1 >= dim.rows)
      }
      cell.east match {
        case Some(eastCell) => eastCell.sameAs(rectMaze.at(r, c+1).get)
        case None => assert(c+1 >= dim.cols)
      }
      cell.west match {
        case Some(westCell) => westCell.sameAs(rectMaze.at(r, c-1).get)
        case None => assert(c-1 < 0)
      }
    }
    val c1 = rectMaze.at(0, 0).get
    val c2 = rectMaze.at(1, 1).get
    rectMaze = c1.link(c2).get
    assert(rectMaze.at(c1.pos).get.linkedCells.size == 1)
    assert(rectMaze.at(c2.pos).get.linkedCells.size == 1)
    assert(rectMaze.at(3,3).get.linkedCells.isEmpty)
  }
}
