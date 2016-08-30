package net.mcarolan.flatmappybird

import java.time.{LocalDate, LocalTime}

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.annotation.tailrec
import scala.util.Random

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

  test("withPrevious") {
    val inputs = (1 to 3).toList
    evalList(CoRoutine.withPrevious(0), inputs) shouldBe List((0, 1), (1, 2), (2, 3))
  }

  test("withPrevious continuation") {
    val inputs = (1 to 100000).toList

    println(LocalTime.now().toString)

    val (nextCo, firstResults) = evalListWithLast(CoRoutine.withPrevious(0), inputs)
    firstResults.last shouldBe (99999, 100000)

    println(LocalTime.now().toString)
    val nextInputs = List(5)

    eval(nextCo, nextInputs) shouldBe Some((100000, 5))
    println(LocalTime.now().toString)
  }


}
