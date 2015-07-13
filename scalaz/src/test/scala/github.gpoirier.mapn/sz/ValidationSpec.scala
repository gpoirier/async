package github.gpoirier.mapn
package sz

import org.scalatest._
import scalaz._
import Scalaz._

class ValidationSpec extends FlatSpec with Matchers {

  type V[X] = ValidationNel[String, X]
  def valid[A](a: A): V[A] = a.success
  def failure[A](message: String): V[A] = message.failureNel

  "mapN" should "work for Validation through the Apply adapter" in {

    val z = mapN {
      val x = use(valid(4))
      val y = use(valid(5))
      x + y
    }
    assert(z == 9.success)
  }

  it should "allow to accumulate errors for ValidationNel" in {
    val result = mapN {
      val x = use(failure[Int]("First"))
      val y = use(valid(4))
      val z = use(failure[Int]("Second"))

      x * y * z
    }

    assert(result === NonEmptyList.nel("First", "Second" :: Nil).failure)
  }
}
