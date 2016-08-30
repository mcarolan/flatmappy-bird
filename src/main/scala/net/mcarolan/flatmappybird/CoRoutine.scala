package net.mcarolan.flatmappybird

trait CoRoutine[Input, Output] { self =>

  def run(input: Input): (Output, CoRoutine[Input, Output])

  def map[OutputT](f: Output => OutputT): CoRoutine[Input, OutputT] =
    new CoRoutine[Input, OutputT] {
      override def run(input: Input): (OutputT, CoRoutine[Input, OutputT]) = {
        val (o, co) = self.run(input)
        (f(o), co.map(f))
      }
    }

  def zip[InputT, OutputT](otherCo: CoRoutine[InputT, OutputT]): CoRoutine[(Input, InputT), (Output, OutputT)] =
    new CoRoutine[(Input, InputT), (Output, OutputT)] {
      override def run(input: (Input, InputT)): ((Output, OutputT), CoRoutine[(Input, InputT), (Output, OutputT)]) =
        input match {
          case (me, other) => {
            val (resMe, nextMe) = self.run(me)
            val (resOther, nextOther) = otherCo.run(other)
            ((resMe, resOther), nextMe.zip(nextOther))
          }
        }
    }

}

object CoRoutine {

  def arr[Input, Output](f: Input => Output): CoRoutine[Input, Output] =
    new CoRoutine[Input, Output] {
      override def run(input: Input): (Output, CoRoutine[Input, Output]) =
        (f(input), arr(f))
    }


  def scan[A, B](f: (A, B) => A, init: A): CoRoutine[B, A] = {
    def step(a: A)(b: B): (A, CoRoutine[B, A]) = {
      val next = f(a, b)

      (next, new CoRoutine[B, A] {
        override def run(input: B): (A, CoRoutine[B, A]) =
          step(next)(input)
      })
    }

    new CoRoutine[B, A] {
      override def run(input: B): (A, CoRoutine[B, A]) =
        step(init)(input)
    }
  }

  def withPrevious[Input](init: Input): CoRoutine[Input, (Input, Input)] = {
    def step(prev: Input): CoRoutine[Input, (Input, Input)] =
      new CoRoutine[Input, (Input, Input)] {
        override def run(input: Input): ((Input, Input), CoRoutine[Input, (Input, Input)]) =
          ((prev, input), step(input))
      }

    step(init)
  }

}
