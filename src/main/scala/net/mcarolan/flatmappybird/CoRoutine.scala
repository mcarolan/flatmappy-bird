package net.mcarolan.flatmappybird

case class CoRoutine[A, B](run: A => (B, CoRoutine[A, B])) {

  def map[D](f: B => D): CoRoutine[A, D] =
    CoRoutine { input =>
      val (o, co) = run(input)
      (f(o), co.map(f))
    }

  def zip[C, D](otherCo: CoRoutine[C, D]): CoRoutine[(A, C), (B, D)] =
    CoRoutine {
      case (me, other) => {
        val (resMe, nextMe) = run(me)
        val (resOther, nextOther) = otherCo.run(other)
        ((resMe, resOther), nextMe.zip(nextOther))
      }
    }

}

object CoRoutine {

  def arr[Input, Output](f: Input => Output): CoRoutine[Input, Output] =
    CoRoutine { input => (f(input), arr(f)) }

  def scan[A, B](f: (A, B) => A, init: A): CoRoutine[B, A] = {
    def step(a: A)(b: B): (A, CoRoutine[B, A]) = {
      val next = f(a, b)

      (next, CoRoutine {
        step(next)
      })
    }

    CoRoutine {
      step(init)
    }
  }

  def withPrevious[Input](init: Input): CoRoutine[Input, (Input, Input)] = {
    def step(prev: Input): CoRoutine[Input, (Input, Input)] =
      CoRoutine { input =>
        ((prev, input), step(input))
      }

    step(init)
  }

}
