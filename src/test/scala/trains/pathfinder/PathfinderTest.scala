package trains.pathfinder

import org.scalatest.{Matchers, FlatSpec}
import trains.model.{Path, Track, TownMap}

class PathfinderTest extends FlatSpec with Matchers {
  "Pathfinder" should "find a path between just two nodes" in {
    val townMap: TownMap = TownMap(Track("AB2"))

    val path: (Int, List[String]) = Pathfinder.shortestPath(townMap, "A", "B")

    path._1 should be (2)
    path._2 should be (List("A", "B"))
  }

  it should "find the shortest path in a complex graph" in {
    val townMap = TownMap(
      Track("AB7"), Track("AC9"), Track("AF14"),
      Track("BC10"), Track("BD15"),
      Track("CD11"), Track("CF2"),
      Track("DE6"),
      Track("EF9"))

    val path: (Int, List[String]) = Pathfinder.shortestPath(townMap, "A", "E")

    path._1 should be (26)
    path._2 should be (List("A", "C", "D", "E"))
  }
}
