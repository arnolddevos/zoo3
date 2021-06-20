package insitu

import scala.util.{Try, Success, Failure}
import scala.concurrent.{Promise, Future, Await}
import scala.concurrent.duration._
import scala.collection.immutable.ListMap

/**
 * Create a test article.  
 */
def test[A](name: String, detail: => String = "")(subject: => A) = 
  Article(name, () => detail, () => subject)

/**
 * Create an asynchronous test article. The action is expected to fullfill a
 * `Promise[A]` within a given `timeout` or 10 milliseconds. 
 *
 * As with a synchronous test, `check(predicate)` will return the result of type `A` 
 * or throw the `action`s exception.
 */
def async[A](name: String, detail: => String = "", timeout: Duration=10.milli)(action: Promise[A] => Unit) =
  test(name, detail) {
    val p = Promise[A]()
    action(p)
    val f = p.future
    Await.ready(f, timeout)
    f.value.get.get
  }

/**
 * Run a suite of tests. Iterate each test `repeat` times. Include assertion tests if `asserts`.
 */
def suite(name: String, asserts: Boolean=true, repeat: Int=1)(action: Reporter ?=> Unit)(using parent: Reporter): Unit = {
  val summaries = test(name) {
    val reporter = Reporter(asserts=asserts, indent=parent.indent+1, repeat=repeat)
    action(using reporter)
    reporter.report()  
  }.check(_.passed)

  summaries.results.foreach(parent.record)
}

/**
 * A `Article[A]` wraps an expression of type `A` and gives it a `name`.  A test 
 * is run by its `check` or `assert` methods. The test `predicate` is passed to these.
 */
class Article[A](val name: String, val detail: () => String, val subject: () => A):

  def attempt(predicate: A => Boolean)(using reporter: Reporter): Try[A] =

    @annotation.tailrec
    def loop(n: Int): Try[A] =
      val t0 = System.currentTimeMillis
      val ta = Try(subject())
      val td = System.currentTimeMillis - t0

      import Outcome._

      val outcome = ta match
        case Success(a) => 
          Try(predicate(a)) match {
            case Success(true)  => Passed
            case Success(false) => Failed(detail())
            case Failure(e)     => CheckThrows(detail(), e)
          }
        case Failure(e)         => TestThrows(detail(), e)
      
      reporter.record(name, td, outcome)
      if(n <= 1) ta else loop(n-1)

    loop(reporter.repeat)

  end attempt

  /**
  * Evaluate the expression under test and return its result.  
  * If it raises an exeception re-raise it otherwise evaluate the `predicate`.
  * As a side effect, record the result.
  */
  def check(predicate: A => Boolean)(using Reporter): A =
    attempt(predicate).get

  /**
  * Evaluate the expression under test.
  * If it raises an exeception catch it otherwise evaluate the predicate.
  * As a side effect, record the result.
  */
  def assert(predicate: A => Boolean)(using reporter: Reporter): Unit =
    if(reporter.asserts) attempt(predicate)

end Article
/*
 * A Reporter accumulates test results. A `given Reporter` must be available in
 * the scope of any `test(...)`
 * 
 */
class Reporter(val asserts: Boolean=true, val indent: Int=0, val repeat: Int=1):

  @volatile
  private var results = ListMap[String, Summary]()

  def record(s: Summary): Unit = synchronized {
    results = results.updated(s.name, results.get(s.name).fold(s)(_.merge(s)))
  }

  def record(name: String, td: Long, outcome: Outcome): Unit =
    val nf = if(outcome.passed) 0 else 1
    record(Summary(name, indent, 1, nf, 0, td, td, td, outcome))

  /**
   *  Snapshot the accumulated results as a Report.
   */
  def report(): Report = Report(results.values.toList)

/**
 *  The result of a single attempt of a single test.
 */
enum Outcome {
  case Passed
  case Failed(detail: String)
  case TestThrows(detail: String, error: Throwable)
  case CheckThrows(detail: String, error: Throwable)

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
        case Failed(detail)          => s"$detail at $index"
        case TestThrows(detail, ex)  => s"$detail ${ex.getMessage}"
        case CheckThrows(detail, ex) => s"$detail ${ex.getMessage}"
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
