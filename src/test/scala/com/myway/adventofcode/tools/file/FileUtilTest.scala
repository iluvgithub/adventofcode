package com.myway.adventofcode.tools.file

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Path, Paths}

class FileUtilTest extends AnyFunSuite with Matchers {

  test("read file") {
    // arrange
    val file = new File(getClass.getClassLoader.getResource("adventofcode/tools/file/sample.txt").getPath)
    // act
    val actual = FileUtil.readFile(file)
    // assert
    actual shouldBe List("a,bc,d", "12,34")
  }

  test("write then read file") {
    // arrange
    val rootUrl = getClass.getClassLoader.getResource(".")
    val rootPath = Paths.get(rootUrl.toURI)
    val relativePath = "adventofcode/tools/file/write.txt"
    val filePath: Path = rootPath.resolve(relativePath)
    val file = filePath.toFile
    // act
    FileUtil.writeFile(file, List("ABC", "123"))
    val actual = FileUtil.readFile(file)
    // assert
    actual shouldBe List("ABC", "123")
  }

}
