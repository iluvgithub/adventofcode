package com.myway.cats.tagless.expr.`final`.algebraicui

import com.myway.cats.tagless.expr.`final`.algebraicui.Controls.{Validation, succeed}
import com.myway.cats.tagless.expr.`final`.algebraicui.SimpleType.Program

import scala.io.StdIn
import scala.util.Try

object SimpleMain {

  def main(arr: Array[String]): Unit = {
    type Program[A] = () => A

    val inOutSimple: Simple = new Simple(println, StdIn.readLine)
    val (name, rating) = Program.quiz[Program](inOutSimple, inOutSimple)()

    println(s"QUIZ result $name, $rating")
  }
}

object SimpleType {
  type Program[A] = () => A

}

case class Simple(printer: String => Unit, reader: () => String) extends Controls[Program] with Layout[Program] {


  def and[A, B](first: Program[A], second: Program[B]): Program[(A, B)] = () => (first(), second())

  def textInput(
                 label: String,
                 placeholder: String,
                 validation: Validation[String] = succeed
               ): Program[String] =
    () => {
      def loop(): String = {
        printer(s"$label (e.g. $placeholder):")
        val input = reader()
        validation(input).fold(
          msg => {
            printer(msg)
            loop()
          },
          value => value
        )
      }

      loop()
    }

  def choice[A](label: String, options: Seq[(String, A)]): Program[A] =
    () => {
      def loop(): A = {
        printer(label)
        options.zipWithIndex.foreach { case ((desc, _), idx) =>
          printer(s"$idx: $desc")
        }
        Try(reader()).fold(
          _ => {
            printer("Please enter a valid number.")
            loop()
          },
          idx => {
            if (idx.toInt >= 0 && idx.toInt < options.size) options(idx.toInt)._2
            else {
              println("Please enter a valid number.")
              loop()
            }
          }
        )
      }

      loop()
    }
}


