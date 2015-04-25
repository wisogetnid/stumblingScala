package trains.model

object Track {
  def apply(trackString: String): Track = {
    new Track(trackString.charAt(0).toString, trackString.charAt(1).toString, trackString.substring(2).toInt)
  }
}

class Track (val start: String, val end: String, val length: Int)