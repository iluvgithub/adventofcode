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

  val size = readLine.toInt
  val angle = readLine.toInt
  val in: List[String] = List.range(0, size).foldLeft[List[String]](Nil)((acc, _) => acc ++ List(readLine))

  in.map(println)
  println("answer")

}
