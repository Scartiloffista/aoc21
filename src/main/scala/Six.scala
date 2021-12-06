package dev.scartiloffista

import Six.dict

object Six extends App {

  

  //val input = "3,4,3,1,2".split(",") map (_.toInt)
  val input = "1,1,3,5,3,1,1,4,1,1,5,2,4,3,1,1,3,1,1,5,5,1,3,2,5,4,1,1,5,1,4,2,1,4,2,1,4,4,1,5,1,4,4,1,1,5,1,5,1,5,1,1,1,5,1,2,5,1,1,3,2,2,2,1,4,1,1,2,4,1,3,1,2,1,3,5,2,3,5,1,1,4,3,3,5,1,5,3,1,2,3,4,1,1,5,4,1,3,4,4,1,2,4,4,1,1,3,5,3,1,2,2,5,1,4,1,3,3,3,3,1,1,2,1,5,3,4,5,1,5,2,5,3,2,1,4,2,1,1,1,4,1,2,1,2,2,4,5,5,5,4,1,4,1,4,2,3,2,3,1,1,2,3,1,1,1,5,2,2,5,3,1,4,1,2,1,1,5,3,1,4,5,1,4,2,1,1,5,1,5,4,1,5,5,2,3,1,3,5,1,1,1,1,3,1,1,4,1,5,2,1,1,3,5,1,1,4,2,1,2,5,2,5,1,1,1,2,3,5,5,1,4,3,2,2,3,2,1,1,4,1,3,5,2,3,1,1,5,1,3,5,1,1,5,5,3,1,3,3,1,2,3,1,5,1,3,2,1,3,1,1,2,3,5,3,5,5,4,3,1,5,1,1,2,3,2,2,1,1,2,1,4,1,2,3,3,3,1,3,5".split(",") map (_.toInt)


  var dictImm = (0 to 8).map{x => x -> input.count(y => y == x)}.toMap
  var dictBig = dictImm.map(x => x._1 -> BigInt.int2bigInt(x._2))


  var dict = collection.mutable.Map(dictImm.toSeq: _*).map(x => x._1 -> BigInt.int2bigInt(x._2))
  
  for(day <- 1 to 256){
    dict = dict.map(x => (x._1 - 1, x._2))
    val foo = dict.getOrElse(8, BigInt.int2bigInt(0))
    dict(8) = dict.getOrElse(8, BigInt.int2bigInt(0)) + dict.getOrElse(-1, BigInt.int2bigInt(0))
    dict(6) = dict.getOrElse(6, BigInt.int2bigInt(0)) + dict.getOrElse(-1, BigInt.int2bigInt(0))
    dict -= -1
    true
  }

  println(dict.values.sum)
}
