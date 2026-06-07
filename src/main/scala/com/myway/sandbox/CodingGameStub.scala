package com.myway.sandbox

import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object CodingGameStub extends App {

  val readline = ???
  def foo(): Unit = {
    Console.err.println("Debug messages...")
  }

  val n = readLine.toInt
  val angle = readLine.toInt
  val in: List[String] = List.range(0, n).foldLeft[List[String]](Nil)((acc, _) => acc ++ List(readLine))

  in.map(println)
  println("answer")

}
