package minio

trait Signature {
  type IO[+E, +A] <: IOops[E, A]
  type Task[+A] = IO[Throwable, A]
  type UIO[+A] = IO[Nothing, A]

  def succeed[A](a: A): IO[Nothing, A]
  def fail[E](e: E): IO[E, Nothing]

  final def fromEither[E, A](ea: Either[E, A]): IO[E, A] = ea.fold(fail, succeed)
  final def fromOption[A](oa: Option[A]): IO[Unit, A] = oa.fold(fail(()))(succeed)
  final val unit: IO[Nothing, Unit] = succeed(())

  def effect[A](effect: => A): IO[Throwable, A]
  def effectTotal[A](effect: => A): IO[Nothing, A]
  def effectBlocking[A](effect: => A): IO[Throwable, A] 
  def effectAsync[E, A](run: (IO[E, A] => Unit) => Any): IO[E, A]
  def effectAsyncMaybe[E, A](run: (IO[E, A] => Unit) => Option[IO[E, A]]): IO[E, A]

  def flatten[E, A](suspense: IO[E, IO[E, A]]): IO[E, A]
  def effectSuspend[A](suspense: => IO[Throwable, A]): IO[Throwable, A]
  def effectSuspendTotal[E, A](suspense: => IO[E, A]): IO[E, A]

  def foreach[E, A, B](as: Iterable[A])(f: A => IO[E, B]): IO[E, List[B]]

  def interrupt: IO[Nothing, Nothing]
  def die(t: => Throwable): IO[Nothing, Nothing]
  def mask[E, A](ea: IO[E, A]): IO[E, A]
  def check: IO[Nothing, Unit]
  def idle: IO[Nothing, Nothing]
  def never = idle

  trait IOops[+E, +A] { this: IO[E, A] =>
    def flatMap[E1 >: E, B](f: A => IO[E1, B]): IO[E1, B]
    def catchAll[F, A1 >: A](f: E => IO[F, A1]): IO[F, A1]
    def map[B](f: A => B): IO[E, B]
    def mapError[E1](f: E => E1): IO[E1, A]
    def zip[E1 >: E, B](other: IO[E1, B]): IO[E1, (A, B)]
    def fold[B](f: E => B, g: A => B): IO[Nothing, B]

    final def either: IO[Nothing, Either[E, A]] = fold(Left(_), Right(_))
    final def option: IO[Nothing, Option[A]] = fold(_ => None, Some(_))
    final def unit: IO[E, Unit] = map(_ => ())
    final def as[B](b: B): IO[E, B] = map(_ => b)
    final def andThen[E1 >: E, B](other: IO[E1, B]): IO[E1, B] = flatMap(_ => other)

    def fork: IO[Nothing, Fiber[E, A]]
    def race[E1 >: E, A1 >: A](other: IO[E1, A1]): IO[E1, A1]
    def raceAll[E1 >: E, A1 >: A](others: Iterable[IO[E1, A1]]): IO[E1, A1]
    
    def ensuring( finalize: IO[Nothing, Any]): IO[E, A]
    def bracket[E1 >: E, B](release: A => IO[Nothing, Any])(use: A => IO[E1, B]): IO[E1, B]
  }

  type Fiber[+E, +A] <: FiberOps[E, A]

  trait FiberOps[+E, +A] { this: Fiber[E, A] =>
    def join: IO[E, A]
    def await: IO[Nothing, Exit[E, A]]
    def result: IO[Nothing, Exit[E, A]]
    def interrupt: IO[Nothing, Exit[E, A]]
    def interruptFork: IO[Nothing, Unit]
    def raceAll[E1 >: E, A1 >: A](fbs: Iterable[Fiber[E1, A1]]): IO[E1, A1]
  }

  type Exit[+E, +A] <: ExitOps[E, A]
  
  trait ExitOps[+E, +A] { this: Exit[E, A] =>
    def propagate: IO[E, A]
    def flatMap[E1 >: E, B](f: A => Exit[E1, B]): Exit[E1, B]
    def map[B](f: A => B): Exit[E, B]
    def option: Option[A]
    def succeeded: Boolean
  }

  type Runtime <: RuntimeOps

  trait RuntimeOps {
    def platform: Platform
    def unsafeRunAsync[E, A](ea: => IO[E, A])(k: Exit[E, A] => Any): Unit
    def unsafeRunSync[E, A](ea: => IO[E, A]): Exit[E, A] 
  }

  def defaultRuntime: Runtime
}
