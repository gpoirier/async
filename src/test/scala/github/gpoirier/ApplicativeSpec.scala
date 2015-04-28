package github.gpoirier

import org.scalatest._

import scala.concurrent._
import duration._

class ApplicativeSpec extends FreeSpec {
  "applicatives" - {
    "works with simple map3 usage" in {
      implicit val ec = ExecutionContext.global
      import Futures._
      import Applicatives._

      val f = applicatives {
        val x = use(Future(7))
        val y = use(Future(13))
        val z = use(Future(5))
        (x + y) * z
      }

      val z = Await.result(f, 1.second)
      assert(z == 100)
    }
  }
}
