package net.mcarolan.flatmappybird

case class CoRoutine[A, B](run: A => (B, CoRoutine[A, B])) {

  def map[C](f: B => C): CoRoutine[A, C] =
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

  def zipWith[D](otherCo: CoRoutine[A, D]): CoRoutine[A, (B, D)] =
    CoRoutine { in =>
      val (resMe, nextMe) = run(in)
      val (resOther, nextOther) = otherCo.run(in)
      ((resMe, resOther), nextMe.zipWith(nextOther))
    }

  def >>>[D](other: CoRoutine[B, D]): CoRoutine[A, D] =
    CoRoutine { a =>
      val (b, next) = run(a)
      val (d, nextOther) = other.run(b)
      (d, next >>> nextOther)
    }

}

object CoRoutine {

  def id[A]: CoRoutine[A, A] = arr(identity)

  def const[A, B](value: B): CoRoutine[A, B] = arr { _ => value }

  def arr[Input, Output](f: Input => Output): CoRoutine[Input, Output] =
    CoRoutine { input => (f(input), arr(f)) }

  def first[A, B, C](co: CoRoutine[A, B]): CoRoutine[(A, C), (B, C)] =
    co.zip(id)

  def second[A, B, C](co: CoRoutine[A, B]): CoRoutine[(C, A), (C, B)] =
    id.zip(co)

  def dropFirst[A, B, C](co: CoRoutine[(A), B]): CoRoutine[(C, A), B] =
    CoRoutine.arr[(C, A), A](_._2) >>> co

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

  def restartWhen[T](co: CoRoutine[T, T], zero: CoRoutine[T, T], test: T => Boolean): CoRoutine[T, T] = {
    def inner[T](first: CoRoutine[T, T], next: CoRoutine[T, T], zero: CoRoutine[T, T], test: T => Boolean): CoRoutine[T, T] =
      CoRoutine { in =>
        val (res, nextNext) = next.run(in)

        if (test(res)) {
          val (resZero, resZeroNext) = (first >>> zero).run(in)
          (resZero, inner(first >>> zero, resZeroNext, zero, test))
        }
        else {
          (res, inner(first, nextNext, zero, test))
        }
      }
    inner(co, co, zero, test)
  }

}
