package net.mcarolan.flatmappybird

import java.time.LocalTime
import java.util.concurrent.TimeUnit

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, FiniteDuration}

class CoRoutineTest extends FunSuite {

  def evalList[Input, Output](co: CoRoutine[Input, Output], list: List[Input]): List[Output] = {
    @tailrec
    def iter(co: CoRoutine[Input, Output], curr: List[Output], remain: List[Input]): List[Output] =
      remain match {
        case Nil => curr.reverse
        case x :: xs => {
          val (o, nextCo) = co.run(x)
          iter(nextCo, curr.::(o), xs)
        }
      }

    iter(co, List.empty, list)
  }

  def evalListWithLast[Input, Output](co: CoRoutine[Input, Output], list: List[Input]): (CoRoutine[Input, Output], List[Output]) = {
    @tailrec
    def iter(co: CoRoutine[Input, Output], curr: List[Output], remain: List[Input]): (CoRoutine[Input, Output], List[Output]) =
      remain match {
        case Nil => (co, curr.reverse)
        case x :: xs => {
          val (o, nextCo) = co.run(x)
          iter(nextCo, curr.::(o), xs)
        }
      }

    iter(co, List.empty, list)
  }

  def eval[Input, Output](co: CoRoutine[Input, Output], inputs: List[Input]): Option[Output] = {
    var current = co
    var i = 0
    var result: Option[Output] = None

    while (i < inputs.size) {
      val (res, next) = current.run(inputs(i))
      current = next
      result = Some(res)
      i = i + 1
    }

    result
  }

  test("arr small example") {
    val co: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    evalList(co, List(1, 2, 3)) shouldBe List(2, 4, 6)
  }

  test("arr large example") {
    val co: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    evalList(co, (1 to 1000000).toList) should have size 1000000
  }

  test("map small example") {
    val double: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    val doubleString: CoRoutine[Int, String] = double.map(_.toString)

    evalList(doubleString, List(1, 2, 3)) shouldBe List("2", "4", "6")
  }

  test("map large example") {
    val double: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    val doubleString: CoRoutine[Int, String] = double.map(_.toString)

    evalList(doubleString, (1 to 1000000).toList) should have size 1000000
  }

  test("accumulation with scan") {
    case class Person(name: String, age: Int)

    val people = List(Person("Martin", 27), Person("Bob", 20), Person("Alice", 21))

    val ageCounter = CoRoutine.scan[Int, Person]((acc, p) => acc + p.age, 0)

    evalList(ageCounter, people) shouldBe List(27, 47, 68)
  }

  test("accumulation with scan large") {
    val list = (0L to 10000L).toList
    val expectedSum = list.sum

    val sumWithCoRoutine = CoRoutine.scan[Long, Long]((acc, next) => acc + next, 0L)

    eval(sumWithCoRoutine, list) shouldBe Some(expectedSum)
  }

  test("zip") {
    val inputA: List[Boolean] = List(true, false, true, true, false)
    val inputB: List[Int] = (1 to 5).toList

    val numberOfTrues: CoRoutine[Boolean, Int] =
      CoRoutine.scan((acc, next) => if (next) acc + 1 else acc, 0)

    val lastEvenAsString: CoRoutine[Int, String] =
      CoRoutine.scan((lastEven, current) => if (current % 2 == 0) current.toString else lastEven, "none")

    val truesAndLastEven: CoRoutine[(Boolean, Int), (Int, String)] =
      numberOfTrues zip lastEvenAsString

    val input = inputA.zip(inputB)

    eval(truesAndLastEven, input) shouldBe Some((3, "4"))
  }

  test("zipWith") {
    val inputs = List(1, 2, 3, 4)

    val coA: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    val coB: CoRoutine[Int, String] = CoRoutine.arr(_.toString)

    val resCo: CoRoutine[Int, (Int, String)] = coA.zipWith(coB)

    evalList(resCo, inputs) shouldBe List((2, "1"), (4, "2"), (6, "3"), (8, "4"))
  }

  test("withPrevious") {
    val inputs = (1 to 3).toList
    evalList(CoRoutine.withPrevious(0), inputs) shouldBe List((0, 1), (1, 2), (2, 3))
  }

  test("withPrevious continuation") {
    val inputs = (1 to 1000).toList

    val (nextCo, firstResults) = evalListWithLast(CoRoutine.withPrevious(0), inputs)
    firstResults.last shouldBe (999, 1000)

    val nextInputs = List(5)

    eval(nextCo, nextInputs) shouldBe Some((1000, 5))
  }

  test("compose with >>>") {
    val intToString: CoRoutine[Int, String] = CoRoutine.arr(_.toString)
    val stringtoSize: CoRoutine[String, Int] = CoRoutine.arr(_.size)

    val charWidth: CoRoutine[Int, Int] = intToString >>> stringtoSize

    evalList(charWidth, List(1, 100, 1000)) shouldBe List(1, 3, 4)
  }

  test("compose with >>> and state tracking") {
    val intToString: CoRoutine[Int, String] = CoRoutine.arr(_.toString)
    val stringtoSize: CoRoutine[String, Int] = CoRoutine.arr(_.size)

    val charWidth: CoRoutine[Int, Int] = intToString >>> stringtoSize

    val charWidthWithPrevious: CoRoutine[Int, (Int, Int)] =
      charWidth >>> CoRoutine.withPrevious(-1)

    evalList(charWidthWithPrevious, List(1, 100, 1000)) shouldBe List((-1, 1), (1, 3), (3, 4))
  }

  test("derivate works") {
    evalList(CoRoutine.derivate[Int], List(1, 2, 3, 4)) shouldBe List(1, 1, 1, 1)
    evalList(CoRoutine.derivate[Int], List(1, 20, 31, 47)) shouldBe List(1, 19, 11, 16)
  }

  test("integrate works") {
    evalList(CoRoutine.integrate[Double], List(1.0, -0.5, 2.2)) shouldBe List(1.0, 0.5, 2.7)
    evalList(CoRoutine.integrate[Double](1.0), List(1.0, -0.5, 2.2)) shouldBe List(2.0, 1.5, 3.7)
  }

  test("Time between calls") {
    def minus(a: LocalTime, b: LocalTime): FiniteDuration =
      FiniteDuration(a.toNanoOfDay - b.toNanoOfDay, TimeUnit.NANOSECONDS)

    val now: LocalTime = LocalTime.now()
    val timeBetweenCalls: CoRoutine[LocalTime, FiniteDuration] = CoRoutine.derivate(now, minus _)

    val input = List(now, now.plusNanos(5000), now.plusSeconds(10), now.plusMinutes(2))
    val expectedResult: List[FiniteDuration] = List(Duration.Zero, Duration(5000, TimeUnit.NANOSECONDS), Duration(9999995000L, TimeUnit.NANOSECONDS), Duration(110, TimeUnit.SECONDS))

    evalList(timeBetweenCalls, input) shouldBe expectedResult
  }

  test("first example") {
    val intToString: CoRoutine[Int, String] = CoRoutine.arr(_.toString)
    val onlyActOnFirst: CoRoutine[(Int, String), (String, String)] = CoRoutine.first(intToString)

    val input = List((3, "x"))
    val expectedOutput = List(("3", "x"))

    evalList(onlyActOnFirst, input) shouldBe expectedOutput
  }

  test("second example") {
    val stringToLength: CoRoutine[String, Int] = CoRoutine.arr(_.size)
    val onlyActOnSecond: CoRoutine[(Int, String), (Int, Int)] = CoRoutine.second(stringToLength)

    val input = List((3, "x"))
    val expectedOutput = List((3, 1))

    evalList(onlyActOnSecond, input) shouldBe expectedOutput
  }

  test("dropFirst") {
    val co: CoRoutine[Int, String] = CoRoutine.arr(_.toString)
    val example: CoRoutine[(String, Int), String] = CoRoutine.dropFirst(co)

    eval(example, List(("Hello", 1))) shouldBe Some("1")
  }

  test("restartWhen that will allow an RNG") {
    val initialPosition = (10, 1)
    val inputs = List((-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1), (-2, 1))
    val expectedOutput = List((8, 1), (6, 1), (4, 1), (2, 1), (8, 2), (6, 2), (4, 2), (2, 2), (8, 3), (6, 3), (4, 3), (2, 3))

    val zero: CoRoutine[(Int, Int), (Int, Int)] =
      CoRoutine.second(CoRoutine.arr(in => in + 1))

    val movePosition: CoRoutine[(Int, Int), (Int, Int)] =
      CoRoutine.first(CoRoutine.integrate(initialPosition._1))

    val movePositionWrap: CoRoutine[(Int, Int), (Int, Int)] =
      CoRoutine.restartWhen(movePosition, zero, _._1 <= 0)

    evalList(movePositionWrap, inputs) shouldBe expectedOutput
  }
}
