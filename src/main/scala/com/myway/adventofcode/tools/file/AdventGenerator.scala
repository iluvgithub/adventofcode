package com.myway.adventofcode.tools.file

import java.io.File

object AdventGenerator {

  def main(args: Array[String]): Unit = {
    val nbDays = 25
    val YEAR = 25
    List.range(0, nbDays).map(_ + 1).foreach(
      day => {
        val fromPattern = s"adventofcode/pattern/DayDAY.scala"
        val data = replace(day, YEAR)(FileUtil.readFile(fromPattern))
        val target = new File(
          s"src/main/scala/com/myway/adventofcode/adv20${YEAR}/Day${day}.scala"
        )
        FileUtil.writeFile(target, data)


        val fromPatternTest = s"adventofcode/pattern/DayDAYTest.scala"
        val dataTest = replace(day, YEAR)(FileUtil.readFile(fromPatternTest))
        val targetTest = new File(
          s"src/test/scala/com/myway/adventofcode/adv20${YEAR}/Day${day}Test.scala"
        )
        FileUtil.writeFile(targetTest, dataTest)

        val targetFile = new File(
          s"src/main/resources/adventofcode/20${YEAR}/day${day}.txt"
        )
        FileUtil.writeFile(targetFile, Nil)
      }
    )
  }

  private def replace(day: Int, year: Int): List[String] => List[String] =
    _.map(_.replace("DAY", s"$day"))
      .map(_.replace("YEAR", s"$year"))

}
