package com.myway.codinggame.solo.medium

object TheFall {

  case class Point(x: Int, y: Int) {
    def display: String = s"$x $y"
  }

  trait Room {

    def fromPos(s: String): Point => Point
  }
  object Type0 extends Room {
    override def fromPos(s: String): Point => Point = identity
  }

  object Type1 extends Room {
    override def fromPos(s: String): Point => Point = p => p.copy(y = p.y + 1)
  }
  object Type2 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "LEFT"  => p.copy(x = p.x + 1)
        case "RIGHT" => p.copy(x = p.x - 1)
        case _       => p
      }
  }

  object Type3 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP" => p.copy(y = p.y + 1)
        case _     => p
      }
  }

  object Type4 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP"   => p.copy(x = p.x - 1)
        case "RIGHT" => p.copy(y = p.y + 1)
        case _       => p
      }
  }

  object Type5 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP"  => p.copy(x = p.x + 1)
        case "LEFT" => p.copy(y = p.y + 1)
        case _      => p
      }
  }

  object Type6 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "LEFT"  => p.copy(x = p.x + 1)
        case "RIGHT" => p.copy(x = p.x - 1)
        case _       => p
      }
  }

  object Type7 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP"   => p.copy(y = p.y + 1)
        case "RIGHT" => p.copy(y = p.y + 1)
        case _       => p
      }
  }

  object Type8 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "LEFT"  => p.copy(y = p.y + 1)
        case "RIGHT" => p.copy(y = p.y + 1)
        case _       => p
      }
  }

  object Type9 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "LEFT" => p.copy(y = p.y + 1)
        case "TOP"  => p.copy(y = p.y + 1)
        case _      => p
      }
  }
  object Type10 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP" => p.copy(x = p.x - 1)
        case _     => p
      }
  }

  object Type11 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "TOP" => p.copy(x = p.x + 1)
        case _     => p
      }
  }

  object Type12 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "RIGHT" => p.copy(y = p.y + 1)
        case _       => p
      }
  }
  object Type13 extends Room {
    override def fromPos(s: String): Point => Point = p =>
      s match {
        case "LEFT" => p.copy(y = p.y + 1)
        case _      => p
      }
  }
  val roomMap: Map[Int, Room] = Map(
    0  -> Type0,
    1  -> Type1,
    2  -> Type2,
    3  -> Type3,
    4  -> Type4,
    5  -> Type5,
    6  -> Type6,
    7  -> Type7,
    8  -> Type8,
    9  -> Type9,
    10 -> Type10,
    11 -> Type11,
    12 -> Type12,
    13 -> Type13
  )

  def init(in: List[String]): List[List[Int]] =
    in.map { s =>
      s.split(" ").map(_.toInt).toList
    }
  def solve(pos: String, map: List[List[Int]]): Point => Point = p => {
    val room = roomMap(map(p.y)(p.x))
    room.fromPos(pos)(p)
  }

}
