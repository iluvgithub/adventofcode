package com.myway.adventofcode.tools.rosetree

import com.myway.adventofcode.tools.rosetree.RoseTree.leaf
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RoseZipperTest extends AnyFunSuite with Matchers {


  val r = RoseTree(1,
    List(
      RoseTree(2,
        List(leaf(3), leaf(4))
      ),
      RoseTree(5,
        List(leaf(6), leaf(7))
      ),
      leaf(8)
    )
  )

  test("down twice, up twice ") {
    // arrange
    val z = new RoseZipper(r)
    // act
    val down2: Option[RoseZipper[Int]] = for {
      d1 <- z.down(1)
      d2 <- d1.down(0)
    } yield d2
    val finalRose: Option[RoseZipper[Int]] = down2.flatMap(_.up).flatMap(_.up)
    val optPath = down2.map(_.upRootPath)

    // assert
    down2.map(_.focus.trace).get shouldBe "6"
    val actual = finalRose.get.focus
    actual.trace shouldBe "1(2(3,4),5(6,7),8)"
    optPath.get shouldBe List(1, 0)
  }


  test("down twice, append up twice ") {
    // arrange
    val z = new RoseZipper(r)
    // act
    val finalRose = z.down(1).
      flatMap(_.down(0)).
      map(_.append(leaf(10))).
      map(_.upRoot)
    // assert
    val actual = finalRose.get.focus
    actual.trace shouldBe "1(2(3,4),5(6(10),7),8)"
  }

  test("modify focus head") {
    // arrange
    val z = new RoseZipper(r)
    // act
    val finalRose = z.modifyFocusHead(42)
    // assert
    val actual = finalRose.focus
    actual.trace shouldBe "42(2(3,4),5(6,7),8)"
  }

  test("find") {
    // arrange
    val z = new RoseZipper(r)
    // act
    val okRoseZ = z.find(5)
    val koRoseZ = z.find(42)
    // assert
    koRoseZ shouldBe None
    okRoseZ.isDefined shouldBe true
    val actual: RoseZipper[Int] = okRoseZ.get
    actual.focus.trace shouldBe "5(6,7)"

  }
}
