package insitu

import scala.util.{Try, Success, Failure}
import scala.concurrent.{Promise, Future, Await}
import scala.concurrent.duration._
import scala.collection.immutable.ListMap

/*
 * A Runner creates tests and accumulates their results. 
 *
 * For example: `val test = Runner(); test("addition")(1 + 1).assert(_ == 2)`
 * Results could be obtained with: `println(test.report.formatted)`
 *
 * The syntax `test(name)(action).assert(predicate)` runs a simple test.  Alternatively, 
 * `test.async(name)(action).assert(predicate)` runs a test on an asynchronous `action` (see 
 * method `async` below).
 *
 * It is convenient to define specialized tests as extensions of `Runner`. For example:
 * `extension[A](test: Runner) def io(name: String)(ea: IO[A]) = test.async(name) { .... }`
 * creates a syntax `test.io(name)(effect).assert(predicate)` that would run a  given IO,
 * collect its result, and inspect it.
 * 
 */
class Runner(val asserts: Boolean=true, indent: Int=0, repeat: Int=1) { test =>

  /**
   * Create a test called `name` for the expression `action`. 
   */
  def apply[A](name: String, trial: => String="")(action: => A) = Test(name, () => trial, () => action)


  /**
   * A `Test[A]` wraps an expression of type `A` and give it a `name`.  A test 
   * is run by its `check` or `assert` methods. The test `predicate` is passed to these.
   */
  class Test[A](val name: String, trial: () => String, action: () => A) {

    def attempt(predicate: A => Boolean): Try[A] = {

      @annotation.tailrec
      def loop(n: Int): Try[A] = {
        val t0 = System.currentTimeMillis
        val ta = Try(action())
        val td = System.currentTimeMillis - t0

        import Outcome._

        val outcome = ta match {
          case Success(a) => 
            Try(predicate(a)) match {
              case Success(true)  => Passed
              case Success(false) => Failed(trial())
              case Failure(e)     => CheckThrows(trial(), e)
            }
          case Failure(e)         => TestThrows(trial(), e)
        }

        val nf = if(outcome.passed) 0 else 1
        record(Summary(name, indent, 1, nf, 0, td, td, td, outcome))
        if(n <= 1) ta else loop(n-1)
      }

      loop(repeat)
    }

    /**
    * Evaluate the expression under test and return its result.  
    * If it raises an exeception re-raise it otherwise evaluate the `predicate`.
    * As a side effect, record the result.
    */
    def check(predicate: A => Boolean): A =
      attempt(predicate).get

    /**
    * Evaluate the expression under test.
    * If it raises an exeception catch it otherwise evaluate the predicate.
    * As a side effect, record the result.
    */
    def assert(predicate: A => Boolean): Unit =
      if(asserts) attempt(predicate)
  }

  /**
   * Create a test called `name` for an asynchronous effect, `action`.  The action is expected to fullfill a
   * `Promise[A]` within a given `timeout` or 10 milliseconds. 
   *
   * As with a synchronous test, `check(predicate)` will return the result of type `A` 
   * or throw the `action`s exception.
   */
   def async[A](name: String, trial: => String="", timeout: Duration=10.milli)(action: Promise[A] => Unit) = {
    def article = {
      val p = Promise[A]()
      action(p)
      val f = p.future
      Await.ready(f, timeout)
      f.value.get.get
    }
    Test(name, () => trial, () => article)
  }

  /**
   * Run a suite of tests. Iterate each test `repeat` times. Include assertion tests if `asserts`.
   */
   def suite(name: String, asserts: Boolean=asserts, repeat: Int=1)(action: Runner => Unit): Unit = {
    val report = test(name) {
      val runner = Runner(asserts=asserts, indent=indent+1, repeat=repeat)
      action(runner)
      runner.report()  
    }.check(_.passed)

    report.results.foreach(record)
  }

  @volatile
  private var results = ListMap[String, Summary]()

  private def record(s: Summary): Unit = synchronized {
    results = results.updated(s.name, results.get(s.name).fold(s)(_.merge(s)))
  }

  /**
   * The 
   */
   def report(): Report = Report(results.values.toList)

}

/**
 *  The result of a single attempt of a single test.
 */
enum Outcome {
  case Passed
  case Failed(trial: String)
  case TestThrows(trial: String, error: Throwable)
  case CheckThrows(trial: String, error: Throwable)

  def passed = 
    this match {
      case Passed => true
      case _      => false
    }
  
  def failed = ! passed
}

/**
 *  The result of one or more attempts at a single test.
 */
case class Summary(
  name: String,    // name of the test
  indent: Int,     // nesting depth of the test in suites
  count: Int,      // number of trials
  fails: Int,      // number of failures
  index: Int,      // number of trials before the given outcome
  tmin: Long,      // minimum of trial durations
  ttot: Long,      // total of trial durations
  tmax: Long,      // maximum of the trial durations
  outcome: Outcome // the first failure or just `Passed`
) {
  def merge(other: Summary) =
    Summary(
      name,
      indent, 
      count+other.count, 
      fails+other.fails,
      if(outcome.failed) index else count+other.index,
      tmin.min(other.tmin), 
      ttot + other.ttot, 
      tmax.max(other.tmax), 
      if(outcome.failed) outcome else other.outcome
    )

  def avg: Double = ttot.toDouble/count/1000.0
  def min: Double = tmin.toDouble/1000.0
  def max: Double = tmax.toDouble/1000.0
}

/**
 *  The results for all attempts of a number of tests .
 */
case class Report(results: List[Summary]) {
  def passed = results.forall(_.outcome.passed)

  /**
   * A formatted listing of these results.
   */
  def formatted: String = results.map(detail).mkString("\n")
    
  private def detail(s: Summary): String = {
    import Outcome._
    import s._

    val spaces1 = " "*(indent+1)
    val spaces2 = " "*(40-name.size-indent).max(1)
    val symbol =
      outcome match {
        case Passed            => "P"
        case Failed(_)         => "F"
        case TestThrows(_, _)  => "!"
        case CheckThrows(_, _) => "?"
      }
    val debug =
      outcome match {
        case Passed                 => ""
        case Failed(trial)          => s"$trial at $index"
        case TestThrows(trial, ex)  => s"$trial ${ex.getMessage}"
        case CheckThrows(trial, ex) => s"$trial ${ex.getMessage}"
      }
    val reset = Console.RESET
    val black = Console.BLACK
    val (color, backg) = 
      outcome match {
        case Passed            => (Console.GREEN, Console.GREEN_B)
        case Failed(_)         => (Console.RED, Console.RED_B)
        case TestThrows(_, _)  => (Console.YELLOW, Console.YELLOW_B)
        case CheckThrows(_, _) => (Console.YELLOW, Console.YELLOW_B)
      }
  
    f"$backg$black $symbol $reset$spaces1$color$name$reset$spaces2$count%3d $min%1.3f $avg%1.3f $max%1.3f $debug"
  }
}
