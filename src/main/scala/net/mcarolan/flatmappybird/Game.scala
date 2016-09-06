package net.mcarolan.flatmappybird

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

case class PipeX(value: Double)
case class PipeGap(value: Double)

case class Pipe(currentX: PipeX, currentGap: PipeGap)

case class ScreenDimensions(width: Double, height: Double)

object Pipe {
  val deltaX: CoRoutine[FiniteDuration, Double] =
    CoRoutine.arr(duration => duration.toMillis / -5.0)
  val width: Double = 50
  val gapSize: Double = 100
  val initialGap: PipeGap = PipeGap(100)
}

case class SystemTime(value: Long) {
  def -(other: SystemTime): FiniteDuration =
    FiniteDuration(value - other.value, TimeUnit.MILLISECONDS)
}

object SystemTime {
  def now(): SystemTime = SystemTime(System.currentTimeMillis())
}

case class Game(screenDimensions: ScreenDimensions) {

  val initialPipeX: Double = screenDimensions.width + 100
  val pipeWraparoundAfter: Double = -50

  val timeSinceLastFrame: CoRoutine[SystemTime, FiniteDuration] = {
    def timeBetween(a: SystemTime, b: SystemTime): FiniteDuration =
      a - b

    CoRoutine.derivate(SystemTime.now(), timeBetween _)
  }

  val pipe: CoRoutine[SystemTime, Pipe] =
    timeSinceLastFrame >>>
    Pipe.deltaX >>>
    CoRoutine.restartWhen(CoRoutine.integrate[Double](initialPipeX), initialPipeX, _ < pipeWraparoundAfter) >>>
    CoRoutine.arr(x => Pipe(PipeX(x), Pipe.initialGap))

  def debug[A]: CoRoutine[A, A] =
    CoRoutine.arr { in =>
      println(s"in: $in")
      in
    }

}
