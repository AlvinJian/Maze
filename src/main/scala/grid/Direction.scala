package grid

sealed trait Direction
case object NorthDir extends Direction
case object SouthDir extends Direction
case object WestDir extends Direction
case object EastDir extends Direction

object Direction {
  def reverse(dir: Direction): Direction = dir match {
    case EastDir => WestDir
    case WestDir => EastDir
    case NorthDir => SouthDir
    case SouthDir => NorthDir
    case _ => dir
  }
}
