package com.myway.codinggame.solo

object MiniCpuInstructionDecoder {

  case class Register(map: Map[String, Long]) {
    def asList: List[Long] = map.keySet.toList.sorted.map(k => map(k))
    def set(k: String, v: String): Register =
      setLong(k, java.lang.Long.parseLong(v, 16))

    def setLong(k: String, v: Long): Register = this.copy(
      this.map - k + (k -> v)
    )
    def getLong(k: String): Long = this.map(k)

  }

  def compute(l: List[String]): Register =
    compute(l, Register(Map("00" -> 0L, "01" -> 0L, "02" -> 0L, "03" -> 0L)))

  def compute(l: List[String], out: Register): Register = l match {
    case Nil => out
    case x :: xs =>
      x match {
        case "01" =>
          val rKey = xs.head
          val v    = xs.tail.head
          compute(xs.tail.tail, out.set(rKey, v))
        case "02" =>
          val xKey = xs.head
          val yKey = xs.tail.head
          compute(xs.tail.tail, out.setLong(xKey, out.getLong(xKey) + out.getLong(yKey)))
        case "03" =>
          val xKey = xs.head
          val yKey = xs.tail.head
          compute(xs.tail.tail, out.setLong(xKey, out.getLong(xKey) - out.getLong(yKey)))
        case "04" =>
          val xKey = xs.head
          val yKey = xs.tail.head
          compute(xs.tail.tail, out.setLong(xKey, out.getLong(xKey) * out.getLong(yKey)))
        case "05" =>
          val xKey = xs.head
          compute(xs.tail, out.setLong(xKey, out.getLong(xKey) + 1))
        case "06" =>
          val xKey = xs.head
          compute(xs.tail, out.setLong(xKey, out.getLong(xKey) - 1))
        case _ =>
          out
      }

  }

  def solve(s: String): List[Long] = {
    val list: List[String] = s.split(" ").toList
    compute(list).asList
  }

}
