package image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import maze.{Cell2D, Maze, Position2D}

trait DrawFunc extends ( (Int, Int, Position2D => RGBColor) => ImmutableImage ) {
  def apply(cellSize: Int, padding: Int, f: Position2D=>RGBColor): ImmutableImage
}

object MazeImage {
  def func(maze: Maze[Cell2D]): DrawFunc = {
    def ret(cellSize: Int, padding: Int, f: Position2D=>RGBColor) = {
      val drawer = Drawer(maze, cellSize)
//      val func: ImmutableImage=>ImmutableImage = {
//        img: ImmutableImage => drawer.drawCells(img, f)
//      }.andThen{
//        img: ImmutableImage => drawer.drawWalls(img)
//      }
      val image = drawer.finalImage(f)// func(drawer.baseImage)
      if (padding > 0) image.pad(padding, java.awt.Color.DARK_GRAY)
      else image
    }
    ret
  }
}
