package github.gpoirier.mapn

import org.scalatest._

import scala.concurrent.ExecutionContext

object ApplicativesSpec {
  implicit val optionApplicative = new ApplicativeLike[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = {
      for {
        a <- fa
        b <- fb
      } yield {
        f(a, b)
      }
    }
  }
}

class ApplicativesSpec extends FlatSpec with Matchers {

  import ApplicativesSpec._

  "mapN" should "support combining only two elements" in {
    val n = mapN {
      val a = use(Some(1))
      val b = use(Option(2))

      a + b
    }

    assert(n === Some(3))
  }

  it should "support a large number of elements" in {
    val n = mapN {
      val a = use(Option(1))
      val b = use(Some(2))
      val c = use(Some(3))
      val d = use(Some(4))
      val e = use(Some(5))
      val f = use(Some(6))
      val g = use(Some(7))

      a * b * c * d * e * f * g
    }

    assert(n === Some((1 to 7).product))
  }
}
