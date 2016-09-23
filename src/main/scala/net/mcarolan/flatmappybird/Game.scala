package net.mcarolan.flatmappybird

import java.util.concurrent.TimeUnit


import scala.concurrent.duration.FiniteDuration

case class Rectangle(x: Double, y: Double, width: Double, height: Double) {
  def left = x
  def right = x + width
  def top = y
  def bottom = y + height

  def intersectsWith(other: Rectangle): Boolean =
    !(other.left > right ||
      other.right < left ||
      other.top > bottom ||
      other.bottom < top)
}

case class Pipe(currentX: Double, currentGap: Double) {

  val topRectangle = Rectangle(currentX, 0.0, Pipe.width, currentGap)
  def bottomRectangle(screenDimensions: ScreenDimensions) = Rectangle(currentX, currentGap + Pipe.gapSize, Pipe.width, screenDimensions.height - Pipe.gapSize - currentGap)

  def toRectangles(screenDimensions: ScreenDimensions): Set[Rectangle] =
    Set(
      topRectangle,
      bottomRectangle(screenDimensions)
    )
}

sealed trait PlayerState
case object Falling extends PlayerState
case object Jumping extends PlayerState

case class Player(currentY : Double) {
  def toRectangle: Rectangle = Rectangle(Player.x, currentY, Player.width, Player.height)
}

case class GameState(player: Player, pipe: Pipe, isGameOver: Boolean)

case class ScreenDimensions(width: Double, height: Double)

object Player {
  val initialY: Double = 50

  val deltaY: CoRoutine[(PlayerState, FiniteDuration), Double] =
    CoRoutine.arr { case (state, duration) =>
      val divisor =
        state match {
          case Falling => 4.5
          case Jumping => -4.5
        }

      duration.toMillis / divisor
    }

  val width: Double = 59
  val height: Double = 48

  val x: Double = 50.0
}

object Pipe {
  val deltaX: CoRoutine[FiniteDuration, Double] =
    CoRoutine.arr(duration => duration.toMillis / -5.0)
  val width: Double = 50
  val gapSize: Double = 150
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
    CoRoutine.restartWhen(moveX, moveGap, { case (currentX, _) => currentX < pipeWraparoundAfter}) >>>
    CoRoutine.arr((Pipe.apply _).tupled)

  val playerJumping: CoRoutine[Set[KeyCode], PlayerState] =
    CoRoutine.arr { keys =>
      if (keys contains KeyCode.space)
        Jumping
      else
        Falling
    }

  val player: CoRoutine[(Set[KeyCode], SystemTime), Player] =
    CoRoutine.first(playerJumping) >>>
    CoRoutine.second(timeSinceLastFrame) >>>
    Player.deltaY >>>
    CoRoutine.integrate(Player.initialY) >>>
    CoRoutine.arr(Player.apply)

  val buildGameState: CoRoutine[(Player, Pipe), GameState] =
    CoRoutine.arr { case (player, pipe) =>
      val playerOffScreen: Boolean = player.currentY < 0 || player.currentY > (screenDimensions.height - Player.height)
      val playerPipeCollide: Boolean =
        pipe.toRectangles(screenDimensions).exists(_.intersectsWith(player.toRectangle))

      GameState(player, pipe, playerOffScreen || playerPipeCollide)
    }

  val game: CoRoutine[(Set[KeyCode], SystemTime), GameState] =
    player.zipWith(CoRoutine.dropFirst(pipe)) >>>
    buildGameState

}
