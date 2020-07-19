package maze

class RectGrid(val rows: Int, val cols: Int) {
  val data: Seq[Position2D] = {
    Vector.from(
      for {
        r <- 0 until rows
        c <- 0 until cols
      } yield Position2D(r, c)
    )
  }

  def at(r: Int, c: Int): Option[Position2D] = {
    if (r >= 0 && r < rows && c >= 0 && c < cols) {
      Some(data(r * rows + c))
    } else None
  }
}
