package github.gpoirier.mapn

import scala.concurrent.{ExecutionContext, Future, Promise}

class FailFastFutureApplicative(implicit ex: ExecutionContext) extends ApplicativeLike[Future] {

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
