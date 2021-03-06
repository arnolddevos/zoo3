package test.minio
import minio.api._
import insitu._
import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.mutable.Buffer

object Main extends App {
  given reporter: Reporter = Reporter()
  val rt = defaultRuntime
  val pl = rt.platform

  def testIO[A, B](name: String)(value: A, expect: A => B)(article: A => IO[Nothing, B])(using Reporter) =
    test(name, detail=s"value=$value"){
      defaultRuntime.unsafeRunSync(article(value)).option
    }.assert {
      case Some(b) if expect(value) == b => true
      case _ => false
    }

  println(s"using runtime $rt main thread is ${Thread.currentThread.getId}")

  test("an OOM error would be fatal") {
    pl.fatal(new OutOfMemoryError())
  }.assert(x => x)

  suite("IO tests", repeat=10) {

    testIO("evaluate a pure success")(42, identity) {
      succeed(_)
    }

    test("evaluate a pure failure") {
      rt.unsafeRunSync(fail(42))
    }.assert {
      ! _.succeeded
    }

    test("run a total effect") {
      var x: Int = 0
      rt.unsafeRunSync(effectTotal{x = 42})
      x
    }.assert {
      _ == 42
    }

    async[Option[Int]]("unsafeRunAsync produces a value") {
      promise =>
        rt.unsafeRunAsync(succeed(42))(ex => promise.success(ex.option))
    }.assert { 
      case Some(42) => true
      case _        => false
    }

    testIO("flatMap pure effects")(42, _ - 1) {
      x => succeed(x).flatMap(n => succeed(n-1))
    }

    testIO("traverse a list of effects")(Iterable(1, 2, 3), identity) {
      xs => foreach(xs)(succeed)
    }

    testIO("map effect")(2, _ + 6) {
      n => succeed(n).map(_ + 6)
    }

    testIO("zip effects")(("the", 2), identity) {
      (s, n) => succeed(s).zip(succeed(n))
    }

    testIO("exercise a queue")("payload", identity) {
      x =>
        for {
          q <- effectTotal(queue[String](10))
          _ <- q.offer(x)
          y <- q.take
        }
        yield y
    }

    testIO("exercise a queue little harder")("payload", identity) {
      x =>
        for {
          q <- effectTotal(queue[String](10))
          _ <- q.offer("")
          _ <- q.offer("")
          _ <- q.take
          _ <- q.offer(x)
          _ <- q.take
          y <- q.take
        }
        yield y
    }

    testIO("exercise just enqueue ops")(Iterable(1, 2, 3), xs => xs.map(_ => ())) {
      xs =>
        for {
          q <- effectTotal(queue[Int](10))
          ys <- foreach(xs)(q.offer)
        }
        yield ys
    }

    testIO("exercise a queue harder")(Iterable(1, 2, 3, 5, 6), identity) {
      xs =>
        for {
          q <- effectTotal(queue[Int](10))
          _ <- foreach(xs)(q.offer)
          ys <- foreach(xs)(_ => q.take)
        }
        yield ys
    }

    testIO("producer and consumer")(1 to 1000, identity) {
      xs =>
        for {
          q  <- effectTotal(queue[Int](10))
          c  <- foreach(xs)(_ => q.take).fork
          _  <- foreach(xs)(q.offer)
          ys <- c.join
        }
        yield ys
    }
  }
  println(reporter.report().formatted)
}
