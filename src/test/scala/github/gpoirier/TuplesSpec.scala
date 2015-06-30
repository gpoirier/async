package github.gpoirier

import org.scalatest._
import scala.language.reflectiveCalls

class TuplesSpec extends FreeSpec {

  object Tuples {
    def instance = TuplesMacro.tuples

    implicit def tuple1[A, B, C] = new Tuple[(A, B), C] {
      type Return = (A, B, C)
      override def flatten(tuple: ((A, B), C)): (A, B, C) = tuple match {
        case ((a, b), c) => (a, b, c)
      }
    }

    trait Tuple[A, B] {
      type Return
      def flatten(tuple: (A, B)): Return
    }

    def flatten[A, B](tuple: (A, B))(implicit A: Tuple[A, B]): A.Return = {
      A.flatten(tuple)
    }
  }

  "." in {
    assert(Tuples.flatten(((1, 2), 3)) == (1, 2, 3))
  }
}
