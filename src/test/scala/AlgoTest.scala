import algorithm.{BinaryTree, BinaryTreeMaze, CellLink, Distance, Sidewinder}
import com.sksamuel.scrimage.nio.PngWriter
import grid.{Grid, GridEx}
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
    val smallGrid = new Grid(4,4)
    val result = solver.solve(smallGrid)
    val s = CellLink.graphToString(result)
    print(s)
    val img = CellLink.graphToImage(result, 64)
    FileHelper.saveToFile(img, writer, s"BinaryTree${ext}", "images")

    val distance = new Distance(result(0)(0))
    for (r <- result.indices) {
      for (c <- result(r).indices) {
        val s = distance.get(result(r)(c)) match {
          case Some(value) => s"${value}"
          case _ => "-"
        }
        print(s + " ")
      }
      println()
    }
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
