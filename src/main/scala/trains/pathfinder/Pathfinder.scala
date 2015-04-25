package trains.pathfinder

import trains.model.{Track, Path, TownMap}

import scala.collection.mutable

object Pathfinder {
  def shortestPath(townMap: TownMap, source: String, destination: String): DijkstraPath[String] = {

    val lookup: Map[String, List[(Int, String)]] = townMap.towns.mapValues(tracks => tracks.map(track => (track.length, track.end))).toMap

    return Dijkstra[String](lookup, List((0, List(source))), destination, Set())
  }

  type DijkstraPath[String] = (Int, List[String])

  def Dijkstra[String](lookup: Map[String, List[(Int, String)]], fringe: List[DijkstraPath[String]], dest: String, visited: Set[String]): DijkstraPath[String] = fringe match {
    case (dist, path) :: fringe_rest => path match {case key :: path_rest =>
      if (key == dest) (dist, path.reverse)
      else {
        val paths = lookup(key).flatMap {case (d, key) => if (!visited.contains(key)) List((dist + d, key :: path)) else Nil}
        val sorted_fringe = (paths ++ fringe_rest).sortWith {case ((d1, _), (d2, _)) => d1 < d2}
        Dijkstra(lookup, sorted_fringe, dest, visited + key)
      }
    }
    case Nil => (0, List())
  }
}
