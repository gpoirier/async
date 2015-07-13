package github.gpoirier.mapn

import org.scalatest._

class TuplesSpec extends FlatSpec with Matchers {
  "merge" should "take values of various size of tuples and create a tuple with all the values combined" in {

    val t1 = (23, 54, 23, 53, 13)
    val x = 45

    assert(Tuples.merge(t1, x) === ((23, 54, 23, 53, 13, 45)))
    assert(Tuples.merge(x, t1) === ((45, 23, 54, 23, 53, 13)))
    assert(Tuples.merge(t1, t1) === ((23, 54, 23, 53, 13, 23, 54, 23, 53, 13)))
  }
}
