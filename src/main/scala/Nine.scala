package dev.scartiloffista

import utils.ReadFile

import scala.collection.immutable.Queue
import scala.language.postfixOps

object Nine extends App {

  val input = ReadFile.getLines(9).map(_.split("")).map(_.map(_.toInt))


  val indices = input.indices.flatMap { y =>
    input.head.indices.map { x =>
      ((x, y), input(y)(x))
    }
  }.toMap


  def fn(i: ((Int, Int), Int), indices: Map[(Int, Int), Int]): Int = {
    val (x, y) = i._1
    val value  = i._2

    val left  = (x - 1, y)
    val right = (x + 1, y)
    val up    = (x, y - 1)
    val down  = (x, y + 1)

    if(value < indices.getOrElse(left, Int.MaxValue)
      && value < indices.getOrElse(right, Int.MaxValue)
      && value < indices.getOrElse(up, Int.MaxValue)
      && value < indices.getOrElse(down, Int.MaxValue)
    )
      value
    else -1

  }

  def fn2(i: ((Int, Int), Int), indices: Map[(Int, Int), Int]): Boolean = {
    val (x, y) = i._1
    val value  = i._2

    val left  = (x - 1, y)
    val right = (x + 1, y)
    val up    = (x, y - 1)
    val down  = (x, y + 1)

    if(value < indices.getOrElse(left, Int.MaxValue)
      && value < indices.getOrElse(right, Int.MaxValue)
      && value < indices.getOrElse(up, Int.MaxValue)
      && value < indices.getOrElse(down, Int.MaxValue)
    )
      true
    else
      false

  }



  val p1 = indices.map { i => fn(i, indices)}
    .filter(_ >= 0)
    .map(_ +1).sum

  val startPositionsP2 = indices.filter(x => fn2(x, indices))

  def bfs(start: (Int, Int), indices: Map[(Int, Int), Int]) = {
    var mappingVisited = indices.map(x => x._1 -> true) + (start -> false)

    var queue: Queue[(Int, Int)] = Queue(start)

    while(queue.nonEmpty){
      val pos = queue.dequeue
      queue = pos._2
      val (x, y) = pos._1
      val left  = (x - 1, y)
      val right = (x + 1, y)
      val up    = (x, y - 1)
      val down  = (x, y + 1)

      for(dir <- Seq(left, right, up, down)) {
        if(mappingVisited.getOrElse(dir, false) && indices(dir) != 9) {
          queue = queue.enqueue(dir)
          mappingVisited = mappingVisited + (dir -> false)
        }
      }
    }
    mappingVisited.values.count(_ == false)
  }

  val p2 = startPositionsP2.map(x => bfs(x._1, indices)).toList.sorted.reverse.slice(0, 3).product
  println(p1)
  println(p2)
}
