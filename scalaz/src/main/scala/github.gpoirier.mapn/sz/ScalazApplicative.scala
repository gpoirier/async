package github.gpoirier.mapn
package sz

import scalaz._

object ScalazApplicative extends ScalazApplicative

trait ScalazApplicative {
  implicit def scalazApply[F[_]](implicit F: Apply[F]): ApplicativeLike[F] = new ApplicativeLike[F] {
    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      F.apply2(fa, fb)(f)
  }
}
