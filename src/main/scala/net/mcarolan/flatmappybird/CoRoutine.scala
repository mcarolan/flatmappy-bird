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

  def >>>[D](other: CoRoutine[B, D]): CoRoutine[A, D] =
    CoRoutine { a =>
      val (b, next) = run(a)
      val (d, nextOther) = other.run(b)
      (d, next >>> nextOther)
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

  def integrate[T: Numeric](implicit numeric: Numeric[T]): CoRoutine[T, T] =
    scan(numeric.plus, numeric.zero)

  def integrate[T: Numeric](zero: T)(implicit numeric: Numeric[T]): CoRoutine[T, T] =
    scan(numeric.plus, zero)

  def derivate[T : Numeric](implicit numeric: Numeric[T]): CoRoutine[T, T] =
    derivate(numeric.zero, numeric.minus _)

  def derivate[T: Numeric](zero: T)(implicit numeric: Numeric[T]): CoRoutine[T, T] =
    derivate(zero, numeric.minus _)

  def derivate[T, V](zero: T, minus: (T, T) => V): CoRoutine[T, V] = {
    val subtract: CoRoutine[(T, T), V] = arr { case (a, b) => minus(b, a) }
    withPrevious(zero) >>> subtract
  }

  def restartWhen[T](co: CoRoutine[T, T], zero: T, test: T => Boolean): CoRoutine[T, T] = {
    def inner[T](first: CoRoutine[T, T], next: CoRoutine[T, T], zero: T, test: T => Boolean): CoRoutine[T, T] =
      CoRoutine { in =>
        val (res, nextNext) = next.run(in)

        if (test(res)) {
          (zero, inner(first, first, zero, test))
        }
        else {
          (res, inner(first, nextNext, zero, test))
        }
      }
    inner(co, co, zero, test)
  }

}
