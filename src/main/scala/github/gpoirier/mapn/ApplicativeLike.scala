package github.gpoirier.mapn

import scala.concurrent.{Future, ExecutionContext}


trait ApplicativeLike[F[_]] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
}

object ApplicativeLike {
  implicit def futureApplicative(implicit ec: ExecutionContext): ApplicativeLike[Future] =
    new FailFastFutureApplicative
}