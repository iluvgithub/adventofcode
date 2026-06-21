package com.myway.cats.tagless.expr.`final`.algebraicui

object Program {
  def quiz[F[_] : Controls : Layout]: F[(String, Int)] =
    implicitly[Layout[F]].and(
      implicitly[Controls[F]].textInput("What is your name?", "John Doe"),
      implicitly[Controls[F]].choice(
        "Tagless final is the greatest thing ever",
        Seq(
          "Strongly disagree" -> 1,
          "Disagree" -> 2,
          "Neutral" -> 3,
          "Agree" -> 4,
          "Strongly agree" -> 5
        )
      )
    )
}


/*
object Program {
  def quiz[F[_]](
                  controls: Controls[F],
                  layout: Layout[F]
                 ): F[(String, Int)] =
    layout.and(
      controls.textInput("What is your name?", "John Doe"),
      controls.choice(
        "Tagless final is the greatest thing ever",
        Seq(
          "Strongly disagree" -> 1,
          "Disagree" -> 2,
          "Neutral" -> 3,
          "Agree" -> 4,
          "Strongly agree" -> 5
        )
      )
    )
}
*/