package dev.scartiloffista

import utils.ReadFile

import scala.collection.mutable.ListBuffer

object Five extends App {


  def getEquation(x1: Int, y1: Int, x2: Int, y2: Int): (Int, Int) => Boolean = {
    (x: Int, y: Int) => {
      val a = y2-y1
      val b = x1-x2
      val c = a*x1 + b*y1
      (a*x + b*y) == c &&
        ((x1 to x2).contains(x) || (x2 to x1).contains(x)) &&
        ((y1 to y2).contains(y) || (y2 to y1).contains(y))
    }
  }

  def checkCovers(x: Int, y: Int, coordsP1: Seq[(Int, Int, Int, Int)]) = {
    val equations = coordsP1.map(x => getEquation(x._1, x._2, x._3, x._4))
    val covers = equations.map(fn => fn(x, y)).count(_ == true)
    covers
  }

  var lines = ReadFile.getLines(5)
  val regexp = """^(\d+),(\d+) -> (\d+),(\d+)$""".r
  val coords = lines.map{case regexp(x1,y1,x2,y2) => (x1.toInt, y1.toInt, x2.toInt, y2.toInt) }

  val max_x = (coords.map(_._1) ++ coords.map(_._3)).max
  val max_y = (coords.map(_._2) ++ coords.map(_._4)).max


  val coordsP1 = coords//.filter(c => c._1 == c._3 || c._2 == c._4)

  val covers = new ListBuffer[Int]

  for (x <- 0 to max_x){
    for(y <- 0 to max_y) {
      val nLinesCovering = checkCovers(x, y, coordsP1)
      covers.addOne(nLinesCovering)
    }
  }

  println(covers.count(_ >= 2))

}
