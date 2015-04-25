package trains.model

import org.scalatest.{Matchers, FlatSpec}

class TrackTest extends FlatSpec with Matchers {

  "A Track" should "be created from a String" in {
    val track = Track("AB5")

    track.start shouldBe "A"
    track.end shouldBe "B"
    track.length shouldBe 5
  }

  it should "throw an exception if the Track String doesn't match [A-Z][A-Z]\\d+" in {
    an [IllegalArgumentException] should be thrownBy Track("")
    an [IllegalArgumentException] should be thrownBy Track("AB")
    an [IllegalArgumentException] should be thrownBy Track("A2")
    an [IllegalArgumentException] should be thrownBy Track("ABC2")
  }
}
