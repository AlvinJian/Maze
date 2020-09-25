package image
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DRect, Cell2DWeave, Maze, Position2D, RectGrid}

class WeaveMazeDrawer(val grid: RectGrid,
                      val maze: Maze[Cell2DWeave],
                      cSize: Int, val inSet: Int) extends Drawer {
  override type M = Maze[Cell2DWeave]

  private val rectDrawer = new RectMazeDrawer(grid, maze, cSize)

  override def cellSize: Int = cSize

  override def baseImage: ImmutableImage = rectDrawer.baseImage.fill(java.awt.Color.GRAY)

  override protected def drawWalls(g2: RichGraphics2D): Unit = {
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    val wallGraphics = g2
    wallGraphics.setColor(wallColor)
    for (cell <- maze) {
      drawOverlayCellWalls(wallGraphics, cell)
      cell.underneath.foreach(hidden => drawHiddenCellWalls(wallGraphics, hidden))
    }
  }

  protected def drawHiddenCellWalls(wallGraphics: RichGraphics2D, cell: Cell2DWeave): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    if (cell.isVerticalLinked) {
      wallGraphics.drawLine(x2, y1, x2, y2)
      wallGraphics.drawLine(x3, y1, x3, y2)
      wallGraphics.drawLine(x2, y3, x2, y4)
      wallGraphics.drawLine(x3, y3, x3, y4)
    } else {
      wallGraphics.drawLine(x1, y2, x2, y2)
      wallGraphics.drawLine(x1, y3, x2, y3)
      wallGraphics.drawLine(x3, y2, x4, y2)
      wallGraphics.drawLine(x3, y3, x4, y3)
    }
  }

  protected def drawOverlayCellWalls(wallGraphics: RichGraphics2D, cell: Cell2DWeave): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    val linked = cell.linked
    if (cell.north.map(north => linked.contains(north)).fold(false)(b => b)) {
      wallGraphics.drawLine(x2, y1, x2, y2)
      wallGraphics.drawLine(x3, y1, x3, y2)
    } else {
      wallGraphics.drawLine(x2, y2, x3, y2)
    }

    if (cell.south.map(south => linked.contains(south)).fold(false)(b => b)) {
      wallGraphics.drawLine(x2, y3, x2, y4)
      wallGraphics.drawLine(x3, y3, x3, y4)
    } else {
      wallGraphics.drawLine(x2, y3, x3, y3)
    }

    if (cell.west.map(w => linked.contains(w)).fold(false)(b => b)) {
      wallGraphics.drawLine(x1, y2, x2, y2)
      wallGraphics.drawLine(x1, y3, x2, y3)
    } else {
      wallGraphics.drawLine(x2, y2, x2, y3)
    }

    if (cell.east.map(e => linked.contains(e)).fold(false)(b => b)) {
      wallGraphics.drawLine(x3, y2, x4, y2)
      wallGraphics.drawLine(x3, y3, x4, y3)
    } else {
      wallGraphics.drawLine(x3, y2, x3, y3)
    }
  }

  override protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val cellGraphics = g2
    for (cell <- maze) {
      drawOverlayCell(cellGraphics, cell, f)
      cell.underneath.foreach(hidden => drawHiddenCell(cellGraphics, hidden, f))
    }
  }

  def drawOverlayCell(cellGraphics: RichGraphics2D, cell: Cell2DWeave, f: Position2D => RGBColor): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    cellGraphics.setColor(f(cell.pos))
    var xleft = x2
    var xright = x3
    var ytop = y2
    var ybottom = y3
    val linked = cell.linked
    if (cell.east.map(e => linked.contains(e)).fold(false)(b => b)) {
      xright = x4
    }
    if (cell.west.map(w => linked.contains(w)).fold(false)(b => b)) {
      xleft = x1
    }
    cellGraphics.fillRect(xleft, ytop, xright - xleft, ybottom - ytop)
    xleft = x2
    xright = x3
    if (cell.north.map(n => linked.contains(n)).fold(false)(b => b)) {
      ytop = y1
    }
    if (cell.south.map(s => linked.contains(s)).fold(false)(b => b)) {
      ybottom = y4
    }
    cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
  }

  def drawHiddenCell(cellGraphics: RichGraphics2D, cell: Cell2DWeave, f: Position2D => RGBColor): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
//    cellGraphics.setColor(f(cell.pos))
    var xleft = x2
    var xright = x3
    var ytop = y2
    var ybottom = y3
    if (cell.isHorizontalLinked) {
      val list = List(cell.west, cell.east).flatten
      val color = if (list.size == 2) {
        Drawer.interpolate(f(list(0).pos), f(list(1).pos))
      } else f(cell.pos)
      cellGraphics.setColor(color)
      xleft = x1; xright = x2;
      ytop = y2; ybottom = y3;
      cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)

      xleft = x3; xright = x4;
      cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
    }

    if (cell.isVerticalLinked) {
      val list = List(cell.north, cell.south).flatten
      val color = if (list.size == 2) {
        Drawer.interpolate(f(list(0).pos), f(list(1).pos))
      } else f(cell.pos)
      cellGraphics.setColor(color)
      ytop = y1; ybottom = y2
      xleft = x2; xright = x3
      cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)

      ytop = y3; ybottom = y4
      cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
    }
  }

  def calcInsetPosition(cell: Cell2DRect):
  (Int, Int, Int, Int, Int, Int, Int, Int) = {
    val (_x1, _y1, _x2, _y2) = rectDrawer.calcRectCellPositions(cell)
    val x1 = _x1
    val x2 = x1 + inSet
    val x3 = _x2 - inSet
    val x4 = _x2

    val y1 = _y1
    val y2 = y1 + inSet
    val y3 = _y2 - inSet
    val y4 = _y2
    (x1, x2, x3, x4, y1, y2, y3, y4)
  }
}
