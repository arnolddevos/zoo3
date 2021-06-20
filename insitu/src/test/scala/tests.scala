package test.insitu
import insitu._
import Outcome._

object Main extends App {
  given reporter: Reporter = Reporter()

  test("tests with the same name get grouped") {
    given reporter: Reporter = Reporter()
    test("test") { 2 + 2 }.assert(_ == 4)
    test("test") { 2 + 2 }.assert(_ == 4)
    reporter.report()
  }.assert(_.results.size == 1)
    
  test("tests with the same name get counted") {
    given reporter: Reporter = Reporter()
    test("test") { 2 + 2 }.assert(_ == 4)
    test("test") { 2 + 2 }.assert(_ == 4)
    reporter.report()
  }.assert(_.results.head.count == 2)

  test("tests with different names displayed separately") {
    given reporter: Reporter = Reporter()
    test("alpha") { 2 + 2 }.assert(_ == 4)
    test("beta") { 2 + 2 }.assert(_ == 4)
    reporter.report()
  }.assert(_.results.size == 2)
  
  test("tests can fail") {
    given reporter: Reporter = Reporter()
    test("failing") { 2 + 2 }.assert(_ == 5)
    reporter.report()
  }.assert(! _.results.head.outcome.passed)
  
  test("tests can succeed") {
    given reporter: Reporter = Reporter()
    test("failing") { 2 + 2 }.assert(_ == 4)
    reporter.report()
  }.assert(_.results.head.outcome.passed)
  
  test("tests can throw an exception") {
    given reporter: Reporter = Reporter()
    test("failing") {
      throw new Exception()
      4
    }.assert(_ == 4)
    reporter.report().results.head.outcome
  }.assert {
    case TestThrows(_, _) => true
    case _            => false
  }
  
  test("assertion can throw an exception") {
    given reporter: Reporter = Reporter()
    test("failing") { 2 + 2 }.assert { r =>
      throw new Exception()
      r == 4
    }
    reporter.report().results.head.outcome
  }.assert {
    case CheckThrows(_, _) => true
    case _                   => false
  }

  test("repetition fails on nth attempt") {
    given reporter: Reporter = Reporter()
    for(i <- 1 to 10) test("integers are less than seven")(i).assert(_ < 7)
    reporter.report().results.head
  }.assert {
    case Summary(_, _, 10, 4, 6, _, _, _, Failed(_)) => true
    case x => false
  }
  
  test("repetition captures failed value") {
    given reporter: Reporter = Reporter()
    for(i <- 1 to 10) test("integers are less than seven", detail = s"$i")(i).assert(_ < 7)
    reporter.report().results.head.outcome
  }.assert {
    case Failed("7") => true
    case x => false
  }

  println(reporter.report().formatted)
}
