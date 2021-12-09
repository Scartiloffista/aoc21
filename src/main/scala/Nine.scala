package dev.scartiloffista

import utils.ReadFile

import scala.collection.immutable.Queue
import scala.language.postfixOps

object Nine extends App {

  val input = ReadFile.getLines(9).map(_.split("")).map(_.map(_.toInt))

  val indices = input.indices.flatMap { y => input.head.indices.map { x => ((x, y), input(y)(x)) } }.toMap

  val p1 = indices.filter(x => findAMin(x, indices)).map(_._2 + 1).sum
  val startPositionsP2 = indices.filter(x => findAMin(x, indices))
  val p2 = startPositionsP2.map(x => bfs(x._1, indices)).toList.sorted.reverse.slice(0, 3).product

  def findAMin(i: ((Int, Int), Int), indices: Map[(Int, Int), Int]): Boolean = {
    val (x, y) = i._1
    val value = i._2

    val f1 = (v: Int) => Seq((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)).map(x => indices.getOrElse(x, Int.MaxValue) > v)

    f1(value).forall(_ == true)

  }

  def bfs(start: (Int, Int), indices: Map[(Int, Int), Int]) = {
    var mappingVisited = indices.map(x => x._1 -> true) + (start -> false)

    var queue: Queue[(Int, Int)] = Queue(start)

    while (queue.nonEmpty) {
      val pos = queue.dequeue
      queue = pos._2
      val (x, y) = pos._1

      for (dir <- Seq((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))) {
        if (mappingVisited.getOrElse(dir, false) && indices(dir) != 9) {
          queue = queue.enqueue(dir)
          mappingVisited = mappingVisited + (dir -> false)
        }
      }
    }
    mappingVisited.values.count(_ == false)
  }

  println(p1)
  println(p2)
}
