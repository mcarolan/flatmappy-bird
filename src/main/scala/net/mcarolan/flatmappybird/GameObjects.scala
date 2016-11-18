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

