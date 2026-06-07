package com.myway.codinggame.solo.easy

object LogicGates {

  case class Logic(key: String, instruction: Combine, refLeft: String, refRight: String) {

    def solve(mapIn: Map[String, String]): String = {
      val out = instruction.combine(
        mapIn(refLeft),
        mapIn(refRight)
      )
      s"$key $out"
    }
  }

  def parse(s: String): Logic = {
    val spl = s.split(" ")
    Logic(
      spl(0),
      choose(spl(1)),
      spl(2),
      spl(3)
    )
  }

  trait Combine {
    def op(l: Char, r: Char): Char
    def combine(l: String, r: String): String =
      l.toList.zip(r.toList).map { case (u, v) => op(u, v) }.mkString
  }

  def choose(s: String): Combine = s match {
    case "AND" =>
      (l: Char, r: Char) => if (l == '-' && r == '-') '-' else '_'

    case "OR" =>
      (l: Char, r: Char) => if (l == '-' || r == '-') '-' else '_'

    case "XOR" =>
      (l: Char, r: Char) => if (l == '-' && r == '-') '_'
      else if (l == '-' || r == '-') '-'
      else '_'

    case "NAND" => new Combine {
      override def op(l: Char, r: Char): Char =
        opp(choose("AND").op(l,r))
    }

    case "NOR" => new Combine {
      override def op(l: Char, r: Char): Char =
        opp(choose("OR").op(l,r))
    }

    case "NXOR" => new Combine {
      override def op(l: Char, r: Char): Char =
        opp(choose("XOR").op(l,r))
    }
  }

  def opp(c:Char) = if(c=='-')'_'else '-'
  def parseInput(s: String): (String, String) = {
    val splt = s.split(" ")
    (splt(0), splt(1))
  }

  def solve(in: List[String]): List[String] = {
    val n                          = in.head.toInt
    val m                          = in.tail.head.toInt
    val inputs: List[String]       = in.drop(2).take(n)
    val mapIn: Map[String, String] = inputs.map(parseInput).toMap
    val logics                     = in.drop(2 + n)

    logics.map(parse).map(_.solve(mapIn))

  }
}
