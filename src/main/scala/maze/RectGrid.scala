package maze

case class RectGrid(rows: Int, cols: Int) extends PlainGrid[Position2D] {
  val data: Seq[Position2D] = {
    Vector.from(
      for {
        r <- 0 until rows
        c <- 0 until cols
      } yield Position2D(r, c)
    )
  }

  override def at(r: Int, c: Int): Option[Position2D] = {
    if (r >= 0 && r < rows && c >= 0 && c < cols) {
      Some(data(r * rows + c))
    } else None
  }

  // not efficient but convenient~
  override def isValid(pos: Position2D): Boolean = data.contains(pos)

  override def iterator: Iterator[Position2D] = data.iterator
}
