package github.gpoirier

import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.concurrent.{Future, ExecutionContext}
import scala.reflect.runtime.universe._

object Applicatives {

  private val obj = tq"Applicatives"

  def use[F[_], T](f: F[T]): T = ???

  def mapN[F[_], A](block: =>A)(implicit F: Applicative[F]): F[A] = macro applicativesImpl

  def applicativesImpl(c: Context)(block: c.Tree)(F: c.Tree) = {
    import c.universe._

    val x = c.untypecheck(block)

    val (declarations, tail) = x.children partition {
      case q"val $ident: $t1 = $obj.use[$ft, $t2]($tree)" => true
      case other =>
        other.foreach {
          case expr @ q"$obj.use[$ft, $t]($tree)" =>
            c.error(expr.pos, "'use' should only be used with the form 'val <ident> = use(<expr>)'")
          case _ =>
        }
        false
    }

    if (declarations.isEmpty)
      c.error(block.pos, "'applicatives' block should contain 'use' expressions, none found.")

    case class Line(
        ident: TermName,
        t: c.Tree,
        tree: c.Tree)

    val lines = for {
      (q"val $ident: $t1 = $obj.use[$ft, $t2]($tree)", index) <- declarations.zipWithIndex
    } yield {
      val t = if (t1.nonEmpty) t1 else t2
      Line(ident, t, tree)
    }

    val types = for (line <- lines) yield line.t
    val fs =  for (line <- lines) yield line.tree
    val idents = for (line <- lines) yield line.ident

    val args = (idents zip types) map { case (ident, t) =>
      q"$ident: $t": c.Tree
    }

    val f = TermName("Applicative$F")

    val merged = fs.reduceLeft { (left, right) =>
      q""" $f.map2($left, $right) { (left, right) => _root_.github.gpoirier.Tuples.merge(left, right) } """
    }
    q"""
      val $f = $F
      $f.map($merged)({ (..$args) =>
        ..$tail
      }.tupled)
    """
  }
}


trait Applicative[F[_]] {

  def unit[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(())) { (a, _) => f(a) }

}

object Applicative {
  implicit def futureApplicative(implicit ec: ExecutionContext): Applicative[Future] =
    new FailFastFutureApplicative
}