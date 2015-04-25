package model

class Marker private(val color: String) {
  override def toString() = color + " marker"
}

object Marker {
  private val colors = Map(
    "green" -> new Marker("green"),
    "blue" -> new Marker("blue"),
    "red" -> new Marker("red")
  )

  def getMarker(color: String) = {
    if (colors.contains(color)) {
      colors(color)
    } else {
      null
    }
  }

  def apply(color : String) : Marker = getMarker(color)
}



