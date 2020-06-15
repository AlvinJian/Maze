package grid

@deprecated
sealed trait Direction
// for cartesian coordinate
case object NorthDir extends Direction
case object SouthDir extends Direction
case object WestDir extends Direction
case object EastDir extends Direction
// for polar coordinate
case object ClockwiseDir extends Direction
case object CounterClockwiseDir extends Direction
case object InwardDir extends Direction
case object OutwardDir extends Direction

@deprecated
object Direction {
  def reverse(dir: Direction): Direction = dir match {
    case EastDir => WestDir
    case WestDir => EastDir
    case NorthDir => SouthDir
    case SouthDir => NorthDir
    case ClockwiseDir => CounterClockwiseDir
    case CounterClockwiseDir => ClockwiseDir
    case InwardDir => OutwardDir
    case OutwardDir => InwardDir
    case _ => ???
  }
}
