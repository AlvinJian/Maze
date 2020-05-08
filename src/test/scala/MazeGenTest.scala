import algorithm.{BinaryTreeMaze, SidewinderMaze}
import com.sksamuel.scrimage.nio.PngWriter
import grid.GridEx
import org.scalatest.FunSuite

import scala.util.Random

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())

  test("Algorithm.BinaryTreeEx") {
    val grid = new GridEx(8,10)
    val generator = new BinaryTreeMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
  }

  test("Algorithm.SidewinderEx") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
  }
}
