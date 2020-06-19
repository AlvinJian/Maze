package cli

import java.io.File

import com.sksamuel.scrimage.nio.PngWriter
import grid.GraphEx
import utils.{FileHelper, ImageUtilsEx}

import scala.annotation.tailrec

object Main extends App {
  val optWs: Option[File] = Wizard.setupWorkspace
  if (optWs.isEmpty || !optWs.get.isDirectory) {
    System.err.println("unexpected error!")
    System.exit(1)
  }
  val padding = Some(5)
  val writer: PngWriter = PngWriter.MaxCompression
  val dirPath = optWs.get.getAbsolutePath

  @tailrec
  def loop(run: Boolean): Unit = {
    if (run) {
      println("======")
      val (grid, cellSize) = Wizard.setupGrid
      println()
      if (Wizard.promptForAnswer("Do you want to see the grid as image? (Y/N): ") == "Y") {
        val image = ImageUtilsEx.creationFunction(new GraphEx(grid))(cellSize, padding)
        val optFile = FileHelper.saveToFile(image, writer, "grid.png", dirPath)
        java.awt.Desktop.getDesktop.open(optFile.get)
      }
      println()
      val maze = Wizard.generateMaze(grid)
      if (Wizard.promptForAnswer("Do you want to see the maze? (Y/N): ") == "Y") {
        val image = ImageUtilsEx.creationFunction(maze)(cellSize, padding)
        val optFile = FileHelper.saveToFile(image, writer, "maze.png", dirPath)
        java.awt.Desktop.getDesktop.open(optFile.get)
      }
      loop(Wizard.promptForAnswer("Continue? (Y/N): ") == "Y")
    }
  }
  loop(true)
}