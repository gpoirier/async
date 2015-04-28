package github.gpoirier

import scala.concurrent.{ExecutionContext, Future, Promise}

object Futures {

//  def futures[A](block: => A)(implicit ec: ExecutionContext) = Applicatives.applicatives[Future, A](block)

  implicit def futureApplicative(implicit ec: ExecutionContext): Applicative[Future] =
    new FailFastFutureApplicative
}

class FailFastFutureApplicative(implicit ex: ExecutionContext) extends Applicative[Future] {
  override def map2[A, B, C](fa: Future[A], fb: Future[B])(f: (A, B) => C): Future[C] = {
    val p = Promise[C]()

    for (f <- List(fa, fb)) {
      f.onFailure {
        case e => p.failure(e)
      }
    }

    p.tryCompleteWith((fa zip fb).map(f.tupled))

    p.future
  }
}

