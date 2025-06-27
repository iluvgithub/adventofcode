package com.myway.adventofcode.tools.file
import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
object FileUtil {

  def readFile(file: File): List[String]  = {
    val bufferedSource = Source.fromFile(file)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()
    lines
  }

  def writeFile(path: File, lines: List[String]): Unit =  {
    val bw = new BufferedWriter(new FileWriter(path))
    for (line <- lines) {
      bw.write(line)
      bw.newLine()
    }
    bw.close()
  }

}
