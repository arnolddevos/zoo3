package test.insitu
import insitu._
import Outcome._

object Main extends App {
  val test = Runner()
  
  test("tests with the same name get grouped") {
    val runner = new Runner()
    runner("test") { 2 + 2 }.assert(_ == 4)
    runner("test") { 2 + 2 }.assert(_ == 4)
    runner.report()
  }.assert(_.results.size == 1)
    
  test("tests with the same name get counted") {
    val runner = new Runner()
    runner("test") { 2 + 2 }.assert(_ == 4)
    runner("test") { 2 + 2 }.assert(_ == 4)
    runner.report()
  }.assert(_.results.head.count == 2)

  test("tests with different names displayed separately") {
    val runner = new Runner()
    runner("alpha") { 2 + 2 }.assert(_ == 4)
    runner("beta") { 2 + 2 }.assert(_ == 4)
    runner.report()
  }.assert(_.results.size == 2)
  
  test("tests can fail") {
    val runner = new Runner()
    runner("failing") { 2 + 2 }.assert(_ == 5)
    runner.report()
  }.assert(! _.results.head.outcome.passed)
  
  test("tests can succeed") {
    val runner = new Runner()
    runner("failing") { 2 + 2 }.assert(_ == 4)
    runner.report()
  }.assert(_.results.head.outcome.passed)
  
  test("tests can throw an exception") {
    val runner = new Runner()
    runner("failing") {
      throw new Exception()
      4
    }.assert(_ == 4)
    runner.report().results.head.outcome
  }.assert {
    case TestThrows(_, _) => true
    case _            => false
  }
  
  test("assertion can throw an exception") {
    val runner = new Runner()
    runner("failing") { 2 + 2 }.assert { r =>
      throw new Exception()
      r == 4
    }
    runner.report().results.head.outcome
  }.assert {
    case CheckThrows(_, _) => true
    case _                   => false
  }

  test("repetition fails on nth attempt") {
    val runner = new Runner()
    for(i <- 1 to 10) runner("integers are less than seven")(i).assert(_ < 7)
    runner.report().results.head
  }.assert {
    case Summary(_, _, 10, 4, 6, _, _, _, Failed(_)) => true
    case x => false
  }
  
  test("repetition captures failed value") {
    val runner = new Runner()
    for(i <- 1 to 10) runner("integers are less than seven", trial = s"$i")(i).assert(_ < 7)
    runner.report().results.head.outcome
  }.assert {
    case Failed("7") => true
    case x => false
  }

  println(test.report().formatted)
}
