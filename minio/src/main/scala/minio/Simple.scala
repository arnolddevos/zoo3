package minio
import scala.annotation._

trait Simple extends Signature { this: Fibers with Synchronization => 

  enum IO[+E, +A] extends IOops[E, A] {

    def flatMap[E1 >: E, B](f: A => IO[E1, B]): IO[E1, B] = FlatMap(this, f)
    def catchAll[F, A1 >: A](f: E => IO[F, A1]): IO[F, A1] = CatchAll(this, f)
    def map[B](f: A => B): IO[E, B] = flatMap(a => succeed(f(a)))
    def mapError[E1](f: E => E1): IO[E1, A] = catchAll(e => fail(f(e)))
    def zip[E1 >: E, B](other: IO[E1, B]): IO[E1, (A, B)] = flatMap(a => other.map(b => (a, b)))
    def fold[B](f: E => B, g: A => B): IO[Nothing, B] = map(g).catchAll(e => succeed(f(e)))

    def race[E1 >: E, A1 >: A](other: IO[E1, A1]): IO[E1, A1] =
      for {
        fb0 <- fork
        fb1 <- other.fork
        a   <- fb0.raceAll(List(fb1))
      }
      yield a

    def raceAll[E1 >: E, A1 >: A](others: Iterable[IO[E1, A1]]): IO[E1, A1] =
      for {
        fb0 <- fork
        fbs <- foreach(others)(_.fork)
        a   <- fb0.raceAll(fbs)
      }
      yield a

    def ensuring(finalize: IO[Nothing, Any]): IO[E, A] =
      mask(
        for {
          c1 <- fork
          ex <- c1.await
          _  <- finalize
          a  <- ex.propagate
        }
        yield a 
      )

    def bracket[E1 >: E, B](release: A => IO[Nothing, Any])(use: A => IO[E1, B]): IO[E1, B] =
      for {
        a <- this
        b <- use(a).ensuring(release(a))
      }
      yield b
  
    def fork: IO[Nothing, Fiber[E, A]] = 
      for {
        fb <- Sync(cx => if(cx.isAlive) cx.fiber.fork(this) else cx.fiber.idle)
        _  <- Sync(cx => Pure(runFiber(fb, cx.platform)))
      }
      yield fb

    case Pure(a: A)
    case Error(e: E)
    case Sync(run: Context => IO[E, A])
    case Async(run: (Context, IO[E, A] => Unit) => IO[E, A])
    case FlatMap[E, A, B](ea: IO[E, A], f: A => IO[E, B]) extends IO[E, B]
    case CatchAll[E, F, A](ea: IO[E, A], f: E => IO[F, A]) extends IO[F, A]
    case MapC(f: Context => Context, ea: IO[E, A])
    case Stop
  }

  import IO._
  import Exit._

  final case class Context(platform: Platform, fiber: Fiber[Any, Any], mask: Boolean) {
    def masked = copy(mask=true)
    def isAlive = mask || fiber.isAlive

    def attempt[A](effect: => A): IO[Throwable, A] = 
      try Pure(effect) 
      catch { 
        case t if ! platform.fatal(t) => Error(t) 
        case t => platform.shutdown(t)
      }

    def sandbox(cont: => Unit): Unit =
      try cont
      catch {
        case t if ! platform.fatal(t) => fiber.die(t)
        case t => platform.shutdown(t)            
      }

    def block[A](effect: => A): IO[Throwable, A] = 
      platform.executeBlocking(attempt(effect))

    def shift(cont: => Unit): Unit =
      platform.executeAsync(sandbox(cont))
  }

  private def runFiber[E, A](fiber: Fiber[E, A], platform: Platform): Unit = {

    def push[E, A](cx: Context, ea: IO[E, A], dx: Int, ke: (E, Int) => Unit, ka: (A, Int) => Unit): Unit =
      if(dx < recurLimit) loop(cx, ea, dx+1, ke, ka)
      else shift(cx, ea, ke, ka)

    def shift[E, A](cx: Context, ea: IO[E, A], ke: (E, Int) => Unit, ka: (A, Int) => Unit): Unit =
      cx.shift(loop(cx, ea, 0, ke, ka))

    @tailrec
    def loop[E, A](cx: Context, ea: IO[E, A], dx: Int, ke: (E, Int) => Unit, ka: (A, Int) => Unit): Unit = 
      ea match {
        case Pure(a)          => ka(a, dx)
        case Error(e)         => ke(e, dx)
        case Sync(run)        => loop(cx, run(cx), dx, ke, ka)
        case Async(run)       => 
          loop(cx, run(cx, shift(cx, _, ke, ka)), dx, ke, ka)
        case FlatMap(ea1, f)  => loop(cx, ea1, dx, ke,
          (a, dx) => push(cx, f(a), dx, ke, ka))
        case CatchAll(ea1, f) => loop(cx, ea1, dx, 
          (e, dx) => push(cx, f(e), dx, ke, ka), ka)
        case MapC(f, ea1)     => loop(f(cx), ea1, dx, ke, ka)
        case Stop             => ()
      }

    shift(Context(platform, fiber, false), fiber.start, (_, _) => (), (_, _) => ())
  }

  private val recurLimit = 100

  def succeed[A](a: A): IO[Nothing, A] = Pure(a)
  def fail[E](e: E): IO[E, Nothing] = Error(e)
  def effect[A](effect: => A): IO[Throwable, A] = Sync(_.attempt(effect))
  def effectTotal[A](effect: => A): IO[Nothing, A] = Sync(_ => Pure(effect))

  def effectBlocking[A](effect: => A): IO[Throwable, A] = 
    Sync(cx => 
      if(cx.isAlive) {
        val ea = cx.block(effect)
        if(cx.isAlive) ea else cx.fiber.idle
      }
      else cx.fiber.idle
    )

  def effectAsync[E, A](run: (IO[E, A] => Unit) => Any): IO[E, A] = 
    Async { (cx, k) =>       
      if(cx.isAlive) run(ea => k(if(cx.isAlive) ea else cx.fiber.idle))
      else k(cx.fiber.idle)
      Stop
    }

  def effectAsyncMaybe[E, A](run: (IO[E, A] => Unit) => Option[IO[E, A]]): IO[E, A] = 
    Async((cx, k) => run(ea => k(if(cx.isAlive) ea else cx.fiber.idle)).getOrElse(Stop))
      
  def flatten[E, A](suspense: IO[E, IO[E, A]]): IO[E, A] =
      suspense.flatMap(identity)

  def effectSuspend[A](suspense: => IO[Throwable, A]): IO[Throwable, A] =
    flatten(effect(suspense))

  def effectSuspendTotal[E, A](suspense: => IO[E, A]): IO[E, A] =
    flatten(effectTotal(suspense))

  def foreach[E, A, B](as: Iterable[A])(f: A => IO[E, B]): IO[E, List[B]] =
    as.foldRight[IO[E, List[B]]](succeed(Nil)) { (a, ebs) => 
      for { 
        b  <- f(a) 
        bs <- ebs
      } 
      yield b :: bs
    }

  val interrupt: IO[Nothing, Nothing]            = Sync(cx => cx.fiber.interruptFork.andThen(cx.fiber.idle))
  def die(t: => Throwable): IO[Nothing, Nothing] = Sync(cx => cx.fiber.die(t).andThen(cx.fiber.idle))
  def mask[E, A](ea: IO[E, A]): IO[E, A]         = MapC(_.masked, ea)
  val check: IO[Nothing, Unit]                   = Sync(cx => if(cx.isAlive) unit else cx.fiber.idle)
  val idle: IO[Nothing, Nothing]                = Sync(_.fiber.idle)

  lazy val defaultRuntime = new Runtime( Platform.default, runFiber )
}
