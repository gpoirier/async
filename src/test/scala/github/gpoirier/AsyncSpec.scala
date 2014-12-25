package github.gpoirier

import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

import org.scalatest.{Matchers, FlatSpec}


class AsyncSpec extends FlatSpec with Matchers  {

  class TestFailure extends Exception

  def sleeper[T](duration: Duration)(result: =>T): Future[T] = Future {
    Thread.sleep(duration.toMillis)
    result
  }

  "for-comprehension" should "fail to start futures in parallel" in {
    val result = for {
      a <- sleeper(100.millis)(10)
      b <- sleeper(100.millis)(20)
      x = a + b
      c <- sleeper(100.millis)(30)
    } yield {
      x + c
    }

    // For comprehensions will not create futures until the previous one completed
    a [TimeoutException] should be thrownBy Await.result(result, 150.millis)
  }

  "async" should "starts futures in parallel" in {

    import Async._

    val result = async {
      val a = use(sleeper(100.millis)(10))
      val b = use(sleeper(100.millis)(20))
      val x = a + b

      val c = use(sleeper(100.millis)(30))
      x + c
    }

    // If the futures are created in parallel,
    // they'll take more than 300 milliseconds
    // to create and fail this test
    try {
      Await.result(result, 150.millis) should be === 60
    } catch {
      case e: TimeoutException => fail(e)
    }
  }

  "async" should "not wait for all futures if one failed" in {
    import Async._

    val result = async {
      val a = use(sleeper(100.millis)(10))
      val b = use(sleeper[Int](1.millis)(throw new TestFailure))
      val x = a + b

      val c = use(sleeper(100.millis)(30))
      x + c
    }

    try {
      a [TestFailure] should be thrownBy Await.result(result, 50.millis)
    } catch {
      case e: TimeoutException => fail(e)
    }
  }

}
