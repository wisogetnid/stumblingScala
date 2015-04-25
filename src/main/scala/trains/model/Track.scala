package trains.model

object Track {
  def apply(trackString: String): Track = {
    val trackPattern: String = """[A-Z][A-Z]\d+"""
    if (! trackString.toUpperCase.matches(trackPattern)) throw new IllegalArgumentException(s"Track does not match pattern ${trackPattern}")
    new Track(trackString.charAt(0).toString, trackString.charAt(1).toString, trackString.substring(2).toInt)
  }
}

class Track (val start: String, val end: String, val length: Int)