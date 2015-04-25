package main.scala

import java.util

import model.Vehicle
import model.Marker

object Sandbox {
  def main(args: Array[String]) = {
    val marker: Marker = Marker.getMarker("blue")
    println(marker + " is " + marker.color)
    println(Marker.getMarker("red")) // can be sweetened with
    println(Marker("green")) // syntactic sugar -> the "apply" method

    val max = Seq(1,42,2,4,7,3,5).reduce {Math.max}
    println(max)
  }
}
