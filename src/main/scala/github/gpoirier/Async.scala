package github.gpoirier

import scala.concurrent.{ExecutionContext, Future}
import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.reflect.runtime.universe._

object Async {

  private val obj = tq"Async"

  def async[T](block: =>T)(implicit ec: ExecutionContext): Future[T] = macro asyncImpl

  def asyncImpl(c: Context)(block: c.Tree)(ec: c.Tree) = {
    import c.universe._

    val x = c.untypecheck(block)

    val (declarations, tail) = x.children partition {
      case q"val $ident: $t1 = $obj.use[$t2]($tree)" => true
      case other =>
        other.foreach {
          case expr @ q"$obj.use[$t]($tree)" =>
            c.error(expr.pos, "'use' should only be used with the form 'val <ident> = use(<expr>)'")
          case _ =>
        }
        false
    }

    case class Line(
        futureTerm: TermName,
        valueTerm: TermName,
        ident: TermName,
        t: c.Tree,
        tree: c.Tree)

    val lines = for {
      (q"val $ident: $t1 = $obj.use[$t2]($tree)", index) <- declarations.zipWithIndex
    } yield {
      val t = if (t1.nonEmpty) t1 else t2
      Line(
        TermName("f" + (index + 1)),
        TermName("v" + (index + 1)),
        ident, t, tree)
    }

    val inits = for (line <- lines) yield {
      import line._
      q"val $futureTerm = $tree"
    }

    val types = for (line <- lines) yield line.t

    val onFailures =  for (line <- lines) yield {
      import line._
      q"$futureTerm onFailure { case e => promise.failure(e) }"
    }

    val yieldElements =  for (line <- lines) yield line.valueTerm

    val caseArgs =  for (line <- lines) yield {
      import line._
      pq"$ident @ (_: $t)"
    }

    val future = lines.foldRight(q"scala.concurrent.Future.successful((..$yieldElements))") { (line, acc) =>
      import line._
      q"$futureTerm.flatMap { case $valueTerm => $acc }($ec)"
    }

   q"""
      {
        ..$inits
        val promise = scala.concurrent.Promise[(..$types)]()
        ..$onFailures
        promise.tryCompleteWith($future)
        promise.future
      }.map({ case (..$caseArgs) =>
        ..$tail
      })($ec)
    """
  }

  def use[T](f: Future[T]): T = ???
}
