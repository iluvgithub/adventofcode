package com.myway.codinggame.solo

object GhostLeg {

  case class Puzzle(cols: List[String],
                    listMaps: List[Map[Int, Boolean]],
                    bottom:List[String]) {
    def solve: List[String] = cols.zipWithIndex.map { case (str, j0) =>
      val outcome = List.range(0, listMaps.size).map(listMaps(_)).foldLeft[Int](j0) { (j, map) =>
        if (map.getOrElse(j - 1, false)) j - 1
        else if (map.getOrElse(j, false)) j + 1
        else j
      }

      s"${str}${bottom(outcome)}"
    }
  }
  def parse(l: List[String]): Puzzle = {
    val row1 = l.head
    val cols = row1.split(" ").toList.filter(_.nonEmpty)
    val tail = l.tail
    val listMaps: List[Map[Int, Boolean]] = tail.map { row =>
      List
        .range(0, (row.length - 1) / 3)
        .foldLeft[Map[Int, Boolean]](Map())((acc, j) =>
          if (row.charAt(j * 3 + 1).equals('-')) acc + (j -> true)
          else acc
        )

    }

    val bottom = l.last.split(" ").toList.filter(_.nonEmpty)
    Puzzle(cols, listMaps,bottom)
  }

  def solve(input: List[String]): List[String] = {
    var puz: Puzzle = parse(input)

    puz.solve
  }
}
