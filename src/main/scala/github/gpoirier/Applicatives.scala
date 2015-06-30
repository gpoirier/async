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
      q"$ident: $t"
    }

    val mapN = TermName("map" + lines.size)

    q"""
      $F.$mapN(..$fs) { (..$args) =>
        ..$tail
      }
    """
  }
}


trait Applicative[F[_]] {

//  def merge[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
//  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, fa) {
//    (a, _) => f(a)
//  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fab = map2(fa, fb) { (a, b) => (a, b) }
    map2(fab, fc) {
      case ((a, b), c) => f(a, b, c)
    }
  }

  def test[A, B, C] = {
    def f1: F[A] = ???
    def f2: F[B] = ???
    def f3: F[C] = ???
//
//    map(merge(f1, merge(f2, f3))) {
//      case (a, (b, c)) => (a, b, c)
//    }
//
    def id[A, B](a: A, b: B) = a -> b
//
//
//
    val x = map2(f1, map2(f2, f3)(id)) {
      case (a, (b, c)) => (a, b, c)
    }

    val y = (1, (2, 4))

    def f: (A, B) => C = ???


  }
//  }

}

object Applicative {
  implicit def futureApplicative(implicit ec: ExecutionContext): Applicative[Future] =
    new FailFastFutureApplicative
}