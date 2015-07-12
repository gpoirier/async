package github.gpoirier

import org.scalatest._

class ApplicativesSpec extends FreeSpec {

  implicit val _ = new Applicative[Option] {

    override def unit[A](a: A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = {
      for {
        a <- fa
        b <- fb
      } yield {
        f(a, b)
      }
    }
  }

  "mapN" - {
    import Applicatives._

    "should support combining only two elements" in {
      val n = mapN {
        val a = use(Some(1))
        val b = use(Some(2))

        a + b
      }

      assert(n == Some(3))
    }

    "should support a large number of elements" in {
      val n = mapN {
        val a = use(Some(1))
        val b = use(Some(2))
        val c = use(Some(3))
        val d = use(Some(4))
        val e = use(Some(5))
        val f = use(Some(6))
        val g = use(Some(7))

        a * b * c * d * e * f * g
      }

      assert(n == Some((1 to 7).reduceLeft(_ * _)))
    }
  }
}
