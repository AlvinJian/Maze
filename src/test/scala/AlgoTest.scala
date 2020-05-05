import algorithm.{BinaryTree, CellLink, Sidewinder}
import com.sksamuel.scrimage.nio.PngWriter
import grid.Grid
import org.scalatest.FunSuite
import utils.FileHelper

import scala.util.Random

class AlgoTest extends FunSuite {
  private val grid = new Grid(8, 10)
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  private var _r = new Random(System.currentTimeMillis())

  test("Algorithm.BinaryTree") {
    val solver = new BinaryTree(_r)
    val result = solver.solve(grid)
    val s = CellLink.graphToString(result)
    print(s)
    val img = CellLink.graphToImage(result, 64)
    FileHelper.saveToFile(img, writer, s"BinaryTree${ext}", "images")
  }

  test("Algorithm.Sidewinder") {
    val solver = new Sidewinder(_r)
    val result = solver.solve(grid)
    val s = CellLink.graphToString(result)
    print(s)
    val img = CellLink.graphToImage(result, 64)
    FileHelper.saveToFile(img, writer, s"Sidewinder$ext", "images")
  }
}
