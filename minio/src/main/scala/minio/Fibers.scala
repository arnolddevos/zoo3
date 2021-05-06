package minio

trait Fibers extends Signature { this: Synchronization =>

  private case class State[+E, +A](phase: Phase[E, A], children: List[Fiber[Any, Any]])

  private enum Phase[+E, +A] {
    case Running
    case Interrupted
    case Terminated(ex: Exit[E, A])
  }

  final class Fiber[+E, +A](ea: IO[E, A]) extends FiberOps[E, A] {

    import Phase._
    import Exit._
    import Status._

    private val state = new Transactor(State[E, A](Running, Nil))

    def isAlive = state.poll.phase == Running

    def start: IO[Nothing, Unit] =
      for {
        x <- ea.fold(Fail(_), Succeed(_))
        _ <- exit(x)
      }
      yield ()

    def fork[E1, A1](ea1: IO[E1, A1]): IO[Nothing, Fiber[E1, A1]] = {
      for {
        child <- effectTotal(new Fiber(ea1))
        _ <- state.transact {
          _ match {
            case State(Running, cs) => Updated(State(Running, child :: cs), unit)
            case _                  => Observed(child.interruptFork)
          }
        }
      } yield child
    }

    private def exit(ex: Exit[E, A]): IO[Nothing, Exit[E, A]] =
      state.transact { s0 =>
        val State(phase, cs) = s0
        phase match {
          case Running => 
            val s1 = State(Terminated(ex), cs)
            Updated(s1, foreach(cs)(_.interruptFork).as(ex))
          case Interrupted =>
            Observed(succeed(Interrupt()))
          case Terminated(ex0) => 
            Observed(succeed(ex0))
        }
      }

    def die(t: Throwable): IO[Nothing, Exit[E, A]] = exit(Die(t))

    def interruptFork: IO[Nothing, Unit] =
      state.transact { s0 =>
        val State(phase, cs) = s0
        phase match {
          case Running => 
            val s1 = State(Interrupted, cs)
            Updated(s1, foreach(cs)(_.interruptFork).as(()))
          case _ => Observed(unit)
        }
      }

    private def resultTx = state.transaction { s0 =>
      s0.phase match {
        case Terminated(ex) => Observed(ex)
        case Interrupted    => Observed(Interrupt())
        case _              => Blocked
      }
    }

    def result = state.transactTotal(resultTx)

    def resultNow = state.transactNow(resultTx)

    private def stop: IO[Nothing, Nothing] = mask(effectAsync(_ => ()))

    def idle: IO[Nothing, Nothing] = state.transact { s0 =>
      val State(phase, cs) = s0
      phase match {
        case Running => Blocked
        case Interrupted => 
          val s1 = State(Terminated(Interrupt()), cs)
          Updated(s1, stop)
        case Terminated(_) => Observed(stop)
      }
    }
  
    private def awaitTx = state.transaction { s0 =>
      val State(phase, cs) = s0
      phase match {
        case Terminated(ex) => Observed((ex, cs))
        case _              => Blocked
      }
    }

    private def disownTx = state.transaction { s0 => 
      val s1 = State(s0.phase, Nil)
      Updated(s1, ())
    }

    def await: IO[Nothing, Exit[E, A]] = 
      for {
        s <- state.transactTotal(awaitTx)
        (ex, cs) = s
        _ <- foreach(cs)(_.await)
        _ <- if(cs.nonEmpty) state.transactTotal(disownTx)
             else unit
      }
      yield ex

    def interrupt: IO[Nothing, Exit[E, A]] =
      for {
        _  <- interruptFork
        ex <- await
      }
      yield ex
      
    def join: IO[E, A] =
      for {
        x <- await
        a <- x.propagate
      }
      yield a

    def raceAll[E1 >: E, A1 >: A](fbs: Iterable[Fiber[E1, A1]]): IO[E1, A1] = {
      val run =
        for {
          arb <- effectTotal { new Arbiter[E1, A1](1 + fbs.size) }
          _   <- arb.register(this)
          _   <- foreach(fbs)(arb.register) 
          ex <- arb.await
        }
        yield ex
  
      val cleanup =
        for {
          _  <- this.interruptFork
          _  <- foreach(fbs)(_.interruptFork)
        }
        yield ()
  
      for {
        ex <- run.ensuring(cleanup)
        a  <- ex.propagate
      }
      yield a
    }
  }

  enum Exit[+E, +A] extends ExitOps[E, A] {

    case Succeed(a: A)
    case Fail(e: E)
    case Die(t: Throwable)
    case Interrupt()

    def propagate: IO[E, A] =
      this match {
        case Succeed(a) => succeed(a)
        case Fail(e)    => fail(e)
        case Interrupt()=> interrupt
        case Die(t)     => die(t)
    }

    def flatMap[E1 >: E, B](f: A => Exit[E1, B]): Exit[E1, B] =
      this match {
        case Succeed(a) => f(a)
        case Fail(e)    => Fail(e)
        case Die(t)     => Die(t)
        case Interrupt()=> Interrupt()
      }

    def map[B](f: A => B): Exit[E, B] = flatMap(a => Succeed(f(a)))

    def option: Option[A] =
      this match {
        case Succeed(a) => Some(a)
        case _          => None
      }

    def succeeded: Boolean =
      this match {
        case Succeed(_) => true
        case _          => false
      }
  }

  class Arbiter[E, A](quota: Int) {
    enum State {
      case Pending(count: Int)
      case Complete(exit: Exit[E,A])
    }
    import State._
    import Status._

    val accum = new Transactor(Pending(quota))

    private def signal(ex: Exit[E, A]) = accum.transaction(
      _ match {
        case Pending(n) if n == 1 || ex.succeeded => Updated(Complete(ex), ())
        case Pending(n)  => Updated(Pending(n-1), ())
        case Complete(_) => Observed(())
      }
    )

    def register(fb: Fiber[E, A]): IO[Nothing, Unit] = effectTotal(
      fb.resultNow( ex => 
        accum.transactNow(signal(ex))(_ => ())
      )
    )

    val await: IO[Nothing, Exit[E, A]] = accum.transactTotal(
      _ match {
        case Pending(_)   => Blocked
        case Complete(ex) => Observed(ex)
      }
    )
  }

  class Runtime(val platform: Platform, runFiber: (Fiber[Any, Any], Platform) => Unit) extends RuntimeOps {
    def unsafeRunAsync[E, A](ea: => IO[E, A])(k: Exit[E, A] => Any): Unit = {
      val fiber = new Fiber(effectSuspendTotal(ea))
      fiber.resultNow(k)
      runFiber(fiber, platform)
    }

    def unsafeRunSync[E, A](ea: => IO[E, A]): Exit[E, A] = {
      val p = new java.util.concurrent.Exchanger[Exit[E, A]]
      unsafeRunAsync(ea)(p.exchange(_))
      p.exchange(null)
    }
  }
}