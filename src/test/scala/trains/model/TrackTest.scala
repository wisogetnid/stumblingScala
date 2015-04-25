package trains.model

import org.scalatest.{Matchers, FlatSpec}

class TrackTest extends FlatSpec with Matchers {

  "A Track" should "be created from a String" in {
    val track = Track("AB5")

    track.start shouldBe "A"
    track.end shouldBe "B"
    track.length shouldBe 5
  }
}
