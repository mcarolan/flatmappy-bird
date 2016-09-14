package net.mcarolan.flatmappybird

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

case class Pipe(currentX: Double, currentGap: Double)

case class Player(currentY : Double)

case class ScreenDimensions(width: Double, height: Double)

object Player {
  val initialY: Double = 50
  val deltaY: CoRoutine[FiniteDuration, Double] =
    CoRoutine.arr(duration => duration.toMillis / 4.5)
  val size: Double = 40.0
}

object Pipe {
  val deltaX: CoRoutine[FiniteDuration, Double] =
    CoRoutine.arr(duration => duration.toMillis / -5.0)
  val width: Double = 50
  val gapSize: Double = 100
  val initialGap: Double = 100
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

  val moveGap: CoRoutine[(Double, Double), (Double, Double)] =
    CoRoutine.second(CoRoutine.arr(currentGap => (currentGap + 250) % screenDimensions.height))

  val moveX: CoRoutine[(Double, Double), (Double, Double)] =
    CoRoutine.first(CoRoutine.integrate[Double](initialPipeX))

  val pipe: CoRoutine[SystemTime, Pipe] =
    timeSinceLastFrame >>>
    Pipe.deltaX >>>
    CoRoutine.arr(deltaX => (deltaX, Pipe.initialGap)) >>>
    CoRoutine.restartWhen(moveX, moveGap, _._1 < pipeWraparoundAfter) >>>
    CoRoutine.arr((Pipe.apply _).tupled)

  val player: CoRoutine[SystemTime, Player] =
    timeSinceLastFrame >>>
    Player.deltaY >>>
    CoRoutine.integrate[Double](Player.initialY) >>>
    CoRoutine.arr(Player.apply)

  val game: CoRoutine[SystemTime, (Pipe, Player)] =
    pipe.zipWith(player)

}
