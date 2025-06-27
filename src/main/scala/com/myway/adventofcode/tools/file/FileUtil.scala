package com.myway.adventofcode.tools.file

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Path, Paths}
import scala.io.Source

object FileUtil {

  def readFile(relativePath: String): List[String] =
    readFile(toFile(relativePath))

  private def toFile(relativePath: String) = {
    val rootUrl = getClass.getClassLoader.getResource(".")
    val rootPath = Paths.get(rootUrl.toURI)
    val filePath: Path = rootPath.resolve(relativePath)
    filePath.toFile
  }

  def readFile(file: File): List[String] = {
    val bufferedSource = Source.fromFile(file)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()
    lines
  }

  def writeFile(relativePath: String, lines: List[String]): Unit =
    writeFile(toFile(relativePath), lines)

  def writeFile(path: File, lines: List[String]): Unit = {
    val bw = new BufferedWriter(new FileWriter(path))
    for (line <- lines) {
      bw.write(line)
      bw.newLine()
    }
    bw.close()
  }


  def main(args: Array[String]): Unit = {
    println("val in = List(")
    val data = FileUtil.readFile("adventofcode/tmp/clipboard.txt")
    data
      .zipWithIndex
      .foreach(s => println(s"\"${s._1}\"${if (s._2 < data.size - 1) "," else ")"}"))

  }
}
