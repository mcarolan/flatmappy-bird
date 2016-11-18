package net.mcarolan.flatmappybird

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

case class Game(screenDimensions: ScreenDimensions) {

  val initialPipeX: Double = screenDimensions.width + 100
  val pipeWraparoundAfter: Double = -50

  val timeSinceLastFrame: CoRoutine[SystemTime, FiniteDuration] = {
    def timeBetween(a: SystemTime, b: SystemTime): FiniteDuration =
      a - b

    CoRoutine.derivate(SystemTime.now(), timeBetween _)
  }

  val pipeNextGap: CoRoutine[(Double, Double), (Double, Double)] =
    CoRoutine.second(CoRoutine.arr(currentGap => (currentGap + 250) % screenDimensions.height))

  val pipeMoveX: CoRoutine[(Double, Double), (Double, Double)] =
    CoRoutine.first(CoRoutine.integrate[Double](initialPipeX))

  val pipe: CoRoutine[SystemTime, Pipe] =
    timeSinceLastFrame >>>
    Pipe.deltaX >>>
    CoRoutine.arr(deltaX => (deltaX, Pipe.initialGap)) >>>
    CoRoutine.restartWhen(pipeMoveX, pipeNextGap, { case (currentX, _) => currentX < pipeWraparoundAfter}) >>>
    CoRoutine.arr((Pipe.apply _).tupled)

  val isPlayerJumpingOrFalling: CoRoutine[Set[KeyCode], PlayerState] =
    CoRoutine.arr { keys =>
      if (keys contains KeyCode.space)
        Jumping
      else
        Falling
    }

  val playerDeltaY: CoRoutine[(PlayerState, FiniteDuration), Double] =
    CoRoutine.arr { case (state, duration) =>
      val divisor =
        state match {
          case Falling => 4.5
          case Jumping => -4.5
        }

      duration.toMillis / divisor
    }

  // case class Player(currentY : Double)
  val player: CoRoutine[(Set[KeyCode], SystemTime), Player] =
    CoRoutine.debug[(Set[KeyCode], SystemTime)]("intial input") >>>
    CoRoutine.first(isPlayerJumpingOrFalling) >>>
    CoRoutine.second(timeSinceLastFrame) >>>
    playerDeltaY >>>
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
