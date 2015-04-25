package model


class Vehicle (val id : Int, val year : Int) {
  def this (id : Int) {
    this (id, 1)
  }

  override def toString : String = "ID:" + id + " year:" + year
}

class Car (override val id : Int, override val year : Int, val fuel : Int) extends Vehicle(id) {
  override def toString : String = "ID:" + id + " year:" + year + " fuel:" + fuel
}
