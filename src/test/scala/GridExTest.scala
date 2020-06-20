import algorithm.DistanceEx
import com.sksamuel.scrimage.nio.PngWriter
import grid.{GraphEx, GridEx, HexGrid, MaskedGrid, PolarGrid, TriangleGrid}
import org.scalatest.FunSuite
import utils.{FileHelper, ImageUtilsEx}

import scala.util.Random

class GridExTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())
  val dir: String = "images"

  test("GridExTest") {
    val grid = new GridEx(8,10)
    val cell0 = grid(0, 0)
    val cell1 = grid(0, 1)
    val cell2 = grid(1, 0)
    val cell3 = grid(1, 1)

    assert(cell0.row == 0 && cell0.col == 0)
    assert(cell1.row == 0 && cell1.col == 1)
    assert(cell2.row == 1 && cell2.col == 0)
    assert(cell3.row == 1 && cell3.col == 1)

    for (cell <- grid) {
      val r = cell.row
      val c = cell.col
      assert(cell.north === {
        if (r-1 >= 0) Some(grid(r-1, c)) else None
      })
      assert(cell.south === {
        if (r+1 < grid.rows) Some(grid(r+1, c)) else None
      })
      assert(cell.east === {
        if (c+1 < grid.cols) Some(grid(r, c+1)) else None
      })
      assert(cell.west === {
        if (c-1 >= 0) Some(grid(r, c-1)) else None
      })
    }
  }

  test("MaskedGridTest") {
    val mask =
      """.x.x
        |....
        |x...
        |x..x""".stripMargin
    val grid = MaskedGrid.from(mask)
    assert(!grid.isValid(grid(0, 1)))
    assert(!grid.isValid(grid(0, 3)))
    assert(!grid.isValid(grid(2, 0)))
    assert(!grid.isValid(grid(3, 0)))
    assert(!grid.isValid(grid(3, 3)))
    for (cell <- grid) assert(grid.isValid(cell))
  }

  test("PolarGridTest") {
    val polarGrid = PolarGrid(20)
    var prevCount = 0
    var sum = 0
    for (r <- 0 until polarGrid.rows) {
      val count = polarGrid.columnCountAt(r)
      sum += count
      println(s"count=${count}")
      assert(count >= prevCount)
      prevCount = count
    }
    assert(sum == polarGrid.size)
    for (polarCell <- polarGrid) {
      val r = polarCell.row
      val c = polarCell.col
      if (r == polarGrid.rows-1) {
        assert(polarCell.outward.isEmpty)
      } else if (r > 0) {
        val rcOuter = polarGrid.columnCountAt(r+1)
        val rc = polarGrid.columnCountAt(r)
        if (rcOuter > rc) assert(polarCell.outward.size == 2)
        else polarCell.outward.size == 1
      } else {
        assert(polarCell.outward.size == polarCell.neighbors.size)
        assert(polarCell.inward.isEmpty)
      }
      assert(polarCell.ccw == polarGrid(r, c+1))
      assert(polarCell.cw == polarGrid(r, c-1))
    }

    val maze = new GraphEx(polarGrid)
    var image = ImageUtilsEx.creationFunction(maze)(32, Some(5))
    var f = FileHelper.saveToFile(image, writer, s"PolarGrid$ext", "images")
    assert(f.isSuccess)
  }

  test("HexGridTest") {
    // TODO dunno how to test so I just make an image and test it visually
    val hexGrid: HexGrid = HexGrid(5, 5)
    val maze: GraphEx = new GraphEx(hexGrid)
    var image = ImageUtilsEx.creationFunction(maze)(32, Some(5))
    var f = FileHelper.saveToFile(image, writer, s"HexGrid$ext", directoryName = "images")
    assert(f.isSuccess)
  }

  test("TriangleGridTest") {
    val triangleGrid = TriangleGrid(8, 12)
    for (cell <- triangleGrid) {
      val upRight: Boolean = (cell.row + cell.col) % 2 == 0
      val (r, c) = (cell.row, cell.col)
      assert(upRight == cell.isUpright)
      // test north
      if (upRight || r-1 < 0) {
        assert(cell.north.isEmpty)
      } else {
        assert(cell.north.get == triangleGrid(r-1, c))
      }
      // test south
      if (!upRight || r+1 >= triangleGrid.rows) {
        assert(cell.south.isEmpty)
      } else {
        assert(cell.south.get == triangleGrid(r+1, c))
      }
      // test east
      if (c+1 < triangleGrid.cols) {
        assert(cell.east.get == triangleGrid(r, c+1))
      } else {
        assert(cell.east.isEmpty)
      }
      // test west
      if (c-1 >= 0) {
        assert(cell.west.get == triangleGrid(r, c-1))
      } else {
        assert(cell.west.isEmpty)
      }
    }
    val image = ImageUtilsEx.creationFunction(new GraphEx(triangleGrid))(32, Some(5))
    val f = FileHelper.saveToFile(image, writer, s"TriangleGrid$ext", dir)
    assert(f.isSuccess)
  }
}
