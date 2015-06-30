package github.gpoirier

import language.experimental.macros
import reflect.macros.whitebox.Context

object TuplesMacro {


  trait Tuple[A, B] {
    type Return
    def flatten(tuple: (A, B)): Return
  }

  def flatten[A, B](tuple: (A, B))(implicit A: Tuple[A, B]): A.Return = {
    A.flatten(tuple)
  }

  def tuples: Any = macro tuplesImpl

  def tuplesImpl(c: Context) = {
    import c.universe._

    q"""
      class TupleMacros {
        implicit def tuple1[A, B, C] = new Tuple[(A, B), C] {
          type Return = (A, B, C)
          override def flatten(tuple: ((A, B), C)): (A, B, C) = tuple match {
            case ((a, b), c) => (a, b, c)
          }
        }
      }
    """
  }

  implicit def tuple1[A, B, C] = new Tuple[(A, B), C] {
    type Return = (A, B, C)
    override def flatten(tuple: ((A, B), C)): (A, B, C) = tuple match {
      case ((a, b), c) => (a, b, c)
    }
  }

}
