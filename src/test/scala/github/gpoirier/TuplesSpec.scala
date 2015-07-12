package github.gpoirier

import org.scalatest._

class TuplesSpec extends FreeSpec {
  "merge" in {

    val t1 = (23, 54, 23, 53, 13)
    val x = 45

    assert(Tuples.merge(t1, x) == (23, 54, 23, 53, 13, 45))
  }
}
