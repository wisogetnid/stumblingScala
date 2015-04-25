package trains.model

import org.scalatest.{Matchers, FlatSpec}

class TownMapTest extends FlatSpec with Matchers {

  "A TownMap" should "contain a single Track" in {
    val expectedTrack: Track = Track("AB2")
    val someTrack: Option[Track] = Some(expectedTrack)
    val townMap: TownMap = TownMap(expectedTrack)

    townMap.towns should contain (("A", List(expectedTrack)))
    townMap.towns should contain (("B", List()))
    townMap.towns.getOrElse("A", fail("no town found")) should contain (expectedTrack)
  }

  it should "return two Tracks starting from the same town" in {
    val firstExpectedTrack: Track = Track("AB2")
    val secondExpectedTrack: Track = Track("AC2")
    val townMap: TownMap = TownMap(firstExpectedTrack, secondExpectedTrack)

    townMap.towns.getOrElse("A", fail("no town found")) should contain (firstExpectedTrack)
    townMap.towns.getOrElse("A", fail("no town found")) should contain (secondExpectedTrack)
  }

  it should "ignore same Tracks with same length" in {
    val firstTrack: Track = Track("AB2")
    val secondTrack: Track = Track("AB2")
    val townMap: TownMap = TownMap(firstTrack, secondTrack)

    townMap.towns.size should be (2)
  }

  it should "turn down same Tracks with different length" in {
    val firstTrack: Track = Track("AB2")
    val secondTrack: Track = Track("AB3")

    an [IllegalArgumentException] should be thrownBy TownMap(firstTrack, secondTrack)
  }
}
