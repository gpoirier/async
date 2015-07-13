package github.gpoirier.mapn.otherpackage

import org.scalatest.{Matchers, FlatSpec}

class ApplicativesOtherpackageSpec extends FlatSpec with Matchers {

  import github.gpoirier.mapn.ApplicativesSpec._

  "mapN" should "work correctly from a different package with precise import" in {
    import github.gpoirier.mapn.Applicatives.{ mapN, use }

    val n = mapN {
      val a = use(Option(1))
      val b = use(Some(2))

      a + b
    }

    assert(n == Some(3))
  }

  it should "work correctly from a different pacakge with package import" in {
    import github.gpoirier.mapn._

    val n = mapN {
      val a: Int = use(Some(1))
      val b: Int = use(None)

      a + b
    }

    assert(n == None)
  }
}
