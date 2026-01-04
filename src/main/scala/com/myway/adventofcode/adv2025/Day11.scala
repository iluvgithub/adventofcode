package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.bag.Bag
import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.list.ListUtil

object Day11 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2025/day11.txt")
    println(solve1(data))
    println(solve2(data))
  }

  def solve1(data: List[String]): Long = {
    val map: Map[String, List[String]] = parseMap(data)
    val youout = loop(map, "out", newBag("you"), 0)
    youout
  }


  def solve2(data: List[String]): Long = {
    val map: Map[String, List[String]] = parseMap(data)
    val svrfft = loop(map, "fft", newBag("svr"), 0)
    val fftdac = loop(map, "dac", newBag("fft"), 0)
    val dacout = loop(map, "out", newBag("dac"), 0)
    val srvdac = loop(map, "dac", newBag("svr"), 0)
    val dacfft = loop(map, "fft", newBag("dac"), 0)
    val fftout = loop(map, "out", newBag("fft"), 0)

    svrfft * fftdac * dacout + srvdac * dacfft * fftout
  }

  private def newBag(str: String): Bag[Trajectory] = new Bag(Trajectory(0, str))

  case class Trajectory(nbSteps: Int, location: String)

  def loop(map: Map[String, List[String]], tgt: String, bag: Bag[Trajectory], nbStp: Int): Long = {
    val keys: Set[Trajectory] = bag.bagMap.keySet.filter(_.nbSteps.equals(nbStp))
    val next: List[(Trajectory, String)] = keys.toList.flatMap(tr => ListUtil.cpl(tr, map.getOrElse(tr.location, Nil)))
    val newBag = next.foldLeft(bag)((acc, t2) => {
      val from: Trajectory = t2._1
      val toLoc: String = t2._2
      val nbSoFar = acc.get(from)
      acc.addMany(Trajectory(nbStp+1, toLoc), nbSoFar)
    })
    if (bag.total < newBag.total) {
      loop(map, tgt, newBag, nbStp + 1)
    }
    else {
      bag.bagMap.keySet.filter(_.location.equals(tgt)).map(k => bag.get(k)).sum
    }

  }

  case class Device(from: String, to: List[String])

  def parseMap(data: List[String]): Map[String, List[String]] = data.map(parse).map(d => d.from -> d.to).toMap

  def parse(s: String): Device = {
    val splt = s.split(":")

    Device(splt(0), splt(1).split(" ").toList.filter(_.length > 2))
  }

}
