package com.myway.codinggame.solo.difficult.variant

object TheResistance {

  final class Node {
    var count: Long = 0L
    var dot: Node   = null // '.'
    var dash: Node  = null // '-'
  }

  final class Trie {

    val root = new Node

    def insert(word: String, value: Long): Unit = {
      var node = root
      var i    = 0

      while (i < word.length) {
        word.charAt(i) match {
          case '.' =>
            if (node.dot == null) node.dot = new Node
            node = node.dot
          case '-' =>
            if (node.dash == null) node.dash = new Node
            node = node.dash
          case _ => // ignore (should not happen)
        }
        i += 1
      }

      node.count += value
    }

    def countWays(message: String): Long = {
      val n    = message.length
      val memo = Array.fill[Long](n + 1)(-1L)

      def dfs(pos: Int): Long = {
        if (pos == n) return 1L
        if (memo(pos) != -1L) return memo(pos)

        var res  = 0L
        var node = root
        var i    = pos

        while (i < n && node != null) {

          node =
            if (message.charAt(i) == '.') node.dot
            else node.dash

          if (node != null && node.count != 0L) {
            res += node.count * dfs(i + 1)
          }

          i += 1
        }

        memo(pos) = res
        res
      }

      dfs(0)
    }
  }

  val morse = Array(
    ".-",
    "-...",
    "-.-.",
    "-..",
    ".",
    "..-.",
    "--.",
    "....",
    "..",
    ".---",
    "-.-",
    ".-..",
    "--",
    "-.",
    "---",
    ".--.",
    "--.-",
    ".-.",
    "...",
    "-",
    "..-",
    "...-",
    ".--",
    "-..-",
    "-.--",
    "--.."
  )

  def idx(c: Char): Int = if (c == '.') 0 else 1

  def split(s: String): List[(String, String)] =
    List.range(1, s.length).map(i => (s.take(i), s.drop(i)))
  def wordToMorse(s: String): String = s.toList.map(c => morse(c - 'A')).mkString
  def solve(input: List[String]): Long = {
    val message = input.head
    val words   = input.drop(2)

    val trie = List.range(0, words.length).foldLeft(new Trie) { (tr, i) =>
      tr.insert(wordToMorse(words(i)), 1L)
      tr
    }

    trie.countWays(message)
  }

}
