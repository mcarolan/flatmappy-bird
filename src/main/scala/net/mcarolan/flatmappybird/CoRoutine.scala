package net.mcarolan.flatmappybird

trait CoRoutine[Input, Output] {

  def run(input: Input): (Output, CoRoutine[Input, Output])

}

object CoRoutine {

  def arr[Input, Output](f: Input => Output): CoRoutine[Input, Output] =
    new CoRoutine[Input, Output] {
      override def run(input: Input): (Output, CoRoutine[Input, Output]) =
        (f(input), arr(f))
    }

}
