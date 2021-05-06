package minio
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.Queue
import annotation.tailrec

trait Synchronization { this: Signature =>

  enum Status[+S, +T] {
    case Updated(state: S, effect: T)
    case Observed(effect: T)
    case Blocked
  }
  import Status._

  type Transaction[S, +T] = S => Status[S, T]

  final class Transactor[S](init: S) {

    def transaction[T](tx: S => Status[S, T]): Transaction[S, T] = tx
      
    def transact[E, A](tx: Transaction[S, IO[E, A]]): IO[E, A] = 
      effectAsyncMaybe[E, A](k => runTx(tx, k))

    def transactTotal[A](tx: Transaction[S, A]): IO[Nothing, A] =
      effectAsyncMaybe[Nothing, A]( k => runTx(tx, k.compose(succeed)).map(succeed))

    def poll: S = cell.get.state

    def transactNow[T](tx: Transaction[S, T])(k: T => Any): Unit =
      runTx(tx, k).foreach(k)
  
    private trait Job {
      type T 
      def phase1: S => Status[S, T]
      def phase2: T => Any
    }

    private object Job {
      def apply[T0](tx: S => Status[S, T0], k: T0 => Any) : Job = 
        new Job {
          type T = T0
          val phase1 = tx
          val phase2 = k
        }
    }

    private case class Cell(state: S, jobs: List[Job])
    private val cell = new AtomicReference(Cell(init, Nil))

    @tailrec
    private def runTx[T](tx: Transaction[S, T], k: T => Any): Option[T] = {
      val c0 = cell.get

      tx(c0.state) match {

        case Updated(s, t) =>  
          if(cell.compareAndSet(c0, Cell(s, Nil))) {
            runJobs(c0.jobs.reverse)
            Some(t)
          }
          else runTx(tx, k)

        case Observed(t) => Some(t)

        case Blocked => 
          if(cell.compareAndSet(c0, Cell(c0.state, Job(tx, k) :: c0.jobs))) None
          else runTx(tx, k)
      } 
    }

    @tailrec
    private def runJobs(jobs: List[Job]): Unit = {
      jobs match {
        case job :: pending =>
          runTx(job.phase1, job.phase2).foreach(job.phase2)
          runJobs(pending)
        case Nil => ()
      }
    }
  }

  trait Gate[-A, +B] {
    def offer(s: A): IO[Nothing, Unit] 
    def take: IO[Nothing, B]
  }

  def semaphore(v0: Long) = new Gate[Long, Long] {
    private val state = new Transactor(v0)

    // P or wait
    val take =
      state.transact { v => 
        if( v > 0 ) Updated(v-1, succeed(v)) 
        else Blocked
      }

    // V or signal
    def offer(i: Long) =
      state.transact { v =>
        Updated(v+i, unit)  
      }
  }

  def barrier() = new Gate[Unit, Long] {
    private val state = new Transactor(0l)

    val take =
      for {
        v0 <- state.transact(v => Observed(succeed(v)))
        v1 <- state.transact { v =>
                if(v > v0) Observed(succeed(v))
                else Blocked
              }
      }
      yield v1

    def offer(u: Unit) =
      state.transact { v => 
        Updated(v + 1, unit)
      }
  }

  def latch[T]() = new Gate[T, T] {
    private val state = new Transactor(None: Option[T])

    val take =
      state.transact { 
        _ match {
          case Some(a) => Observed(succeed(a))
          case None    => Blocked
        }
      }

    def offer(t: T) =
      state.transact {
        _ match {
          case Some(_) => Observed(unit)
          case None    => Updated(Some(t), unit)
        }
      } 
  }

  def queue[T](quota: Int) = new Gate[T, T] {
    private val state = new Transactor(Queue.empty[T])

    val take =
      state.transact { q => 
        if( ! q.isEmpty ) Updated( q.tail, succeed(q.head))
        else Blocked 
      }

    def offer(t: T) =
      state.transact { q =>
        if( q.length < quota ) Updated(q.enqueue(t), unit)
        else Blocked
      }
  }
}