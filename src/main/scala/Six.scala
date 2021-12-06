package dev.scartiloffista

import utils.ReadFile

import scala.annotation.tailrec

object Six extends App {

  def generateDict(dict: Map[Int, BigInt]) = {
    val foo = dict.map(x => x._1 - 1 -> x._2)
    val foo1 = foo + (8 -> foo.getOrElse(8, BigInt.int2bigInt(0)).+(foo.getOrElse(-1, BigInt.int2bigInt(0))))
    val foo2 = foo1 + (6 -> foo1.getOrElse(6, BigInt.int2bigInt(0)).+(foo1.getOrElse(-1, BigInt.int2bigInt(0))))
    val foo3 = foo2 - -1
    foo3
  }

  @tailrec
  def iterateDict(dict: Map[Int, BigInt], day: Int): BigInt = {
    day match {
      case 0 => dict.values.sum
      case _ => iterateDict(generateDict(dict), day - 1)
    }
  }

  val input = ReadFile.getLines(6).head.split(",").map(_.toInt)
  var dict = (0 to 8).map { x => x -> BigInt.int2bigInt(input.count(y => y == x)) }.toMap

  var result1 = iterateDict(dict, 80)
  var result2 = iterateDict(dict, 256)

  println(result1)
  println(result2)

}