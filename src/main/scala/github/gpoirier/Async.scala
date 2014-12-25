package github.gpoirier

import scala.concurrent.{ExecutionContext, Promise, Future}
import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.reflect.runtime.universe._

object Async {

  private val clazz = Async.getClass.getName

  def zip[A, B](fa: Future[A], fb: Future[B])(implicit ec: ExecutionContext): Future[(A, B)] = {
    val promise = Promise[((A, B))]()
    promise.tryCompleteWith(fa zip fb)
    List(fa, fb) foreach { _.onFailure { case e => promise.failure(e) } }

    promise.future
  }

  def zip[A, B, C](fa: Future[A], fb: Future[B], fc: Future[C])(implicit ec: ExecutionContext): Future[(A, B, C)] = {
    val promise = Promise[((A, B), C)]()
    promise.tryCompleteWith(fa zip fb zip fc)
    List(fa, fb, fc) foreach { _.onFailure { case e => promise.failure(e) } }

    promise.future.map { case ((a, b), c) => (a, b, c) }
  }

  def zip[A, B, C, D](fa: Future[A], fb: Future[B], fc: Future[C], fd: Future[D])
                     (implicit ec: ExecutionContext): Future[(A, B, C, D)] = {
    val promise = Promise[(((A, B), C), D)]()
    promise.tryCompleteWith(fa zip fb zip fc zip fd)
    List(fa, fb, fc, fd) foreach { _.onFailure { case e => promise.failure(e) } }

    promise.future.map { case (((a, b), c), d) => (a, b, c, d) }
  }

  def async[T](block: =>T)(implicit ec: ExecutionContext): Future[T] = macro asyncImpl

  def asyncImpl(c: Context)(block: c.Tree)(ec: c.Tree) = {
    import c.universe._

    val x = c.untypecheck(block)

    val (declarations, tail) = x.children partition {
      case q"val $ident = $clazz.use[$t]($tree)" => true
      case other =>
        other.foreach {
          case expr @ q"$clazz.use[$t]($tree)" =>
            c.error(expr.pos, "'use' should only be used with the form 'val $ident = use($expr)'")
          case _ =>
        }
        false
    }

    val (zipArgs, caseArgs) = (for {
      q"val $ident = $clazz.use[$t]($tree)" <- declarations
    } yield {
      (tree, pq"$ident @ (_: $t)")
    }).unzip

    q"""
      zip(..$zipArgs)($ec).map { case (..$caseArgs) =>
        ..$tail
      }
    """
  }

  def use[T](f: Future[T]): T = ???

}
