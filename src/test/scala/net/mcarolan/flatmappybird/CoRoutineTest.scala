package net.mcarolan.flatmappybird

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.annotation.tailrec

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

  test("arr small example") {
    val co: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    evalList(co, List(1, 2, 3)) shouldBe List(2, 4, 6)
  }

  test("arr large example") {
    val co: CoRoutine[Int, Int] = CoRoutine.arr(_ * 2)
    evalList(co, (1 to 1000000).toList) should have size(1000000)
  }



}
