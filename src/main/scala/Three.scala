package dev.scartiloffista

import utils.ReadFile

import scala.util.control.Breaks.{break, breakable}



object Three extends App {

  def createMask(s: Seq[String]): Array[Int] = {
    s.foldLeft(new Array[Int](s.head.length)){ (acc, s) =>
      s.map(s => if(s=='0') -1 else 1).toArray.zip(acc).map(x=> x._1 + x._2)
    }
  }

  val instrs = ReadFile.getLines(3)
  val len = instrs.head.length
  val vals = createMask(instrs)
  // refactor with fold

  val gammaStr = vals.foldLeft("")((s,c) => if(c>0) s.concat("1") else s.concat("0"))
  val epsilonStr = gammaStr.map(x => (1-x.toString.toInt).toString).mkString("")

  val epsilon = Integer.parseInt(epsilonStr, 2)
  val gamma = Integer.parseInt(gammaStr, 2)


//
  var instrs2 = instrs
//
  breakable {for(i <- 0 until len){
    val mask2 = createMask(instrs2)
    val maskOxygen = mask2.map(_>=0).map(if(_) 1 else 0)
    instrs2 = instrs2.filter(x=> x(i).toString.toInt == maskOxygen(i))
    if(instrs2.length == 1){
      break
    }
  }}
  val oxygen = Integer.parseInt(instrs2.head, 2)

  var instrs3 = instrs

  breakable{
    for (i <- 0 until len) {
      val mask2 = createMask(instrs3)
      val maskCO2 = mask2.map(_ < 0).map(if (_) 1 else 0)
      instrs3 = instrs3.filter(x => x(i).toString.toInt == maskCO2(i))
      if (instrs3.length == 1)
        break
    }
  }
  val CO2 = Integer.parseInt(instrs3.head, 2)
//
//
  println(epsilon*gamma)
  println(CO2 * oxygen)


}
