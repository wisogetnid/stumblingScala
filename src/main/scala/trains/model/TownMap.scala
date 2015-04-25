package trains.model

object TownMap {
  def apply(tracks: Track*): TownMap = {
    val duplicatedTracks: Map[String, String] = tracks.groupBy(track => (track.start, track.end))
      .collect{case (group, tracks) if tracks.size > 1 && tracks.groupBy(_.length).keys.size > 1 => group}
    if (duplicatedTracks.keys.size > 0) throw new IllegalArgumentException(s"Following Tracks are duplicated: ${duplicatedTracks}")

    val keys: Seq[String] = (tracks.map(track => track.start) ++ tracks.map(track => track.end)).distinct
    val map = keys.map(key => (key, tracks.filter(track => track.start equals(key)).toList)).toMap

    new TownMap(map)
  }
}

class TownMap(val towns: Map[String, List[Track]])


