package github.gpoirier

import language.experimental.macros
import reflect.macros.whitebox.Context

object Tuples {
  def merge(arg1: Any, arg2: Any): Any = macro mergeImpl

  def mergeImpl(c: Context)(arg1: c.Tree, arg2: c.Tree) = {
    import c.universe._

    val t1 = arg1.tpe.typeSymbol.fullName
    val t2 = arg2.tpe.typeSymbol.fullName

    val R = raw"scala\.Tuple(\d+)".r

    val left = t1 match {
      case R(x) =>
        for (i <- 1 to x.toInt) yield {
          val field = TermName("_" + i)
          q"$arg1.$field"
        }
      case _ =>
        List(q"$arg1")
    }
    val right = t2 match {
      case R(x) =>
        for (i <- 1 to x.toInt) yield {
          val field = TermName("_" + i)
          q"$arg2.$field"
        }
      case _ =>
        List(q"$arg2")
    }

    q"(..${left ++ right})"
  }

}
