package net.mcarolan.flatmappybird

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLDocument

import scala.scalajs.js.annotation.JSExport

@JSExport
object UI {

  case class Colour(value: String)

  def toRectangles(screenDimensions: ScreenDimensions, player: Player, pipe: Pipe): Map[Rectangle, Colour] =
    pipe.toRectangles(screenDimensions).map(_ -> Colour("green")) + (player.toRectangle -> Colour("red")) toMap

  def doFrame(co: CoRoutine[(Set[KeyCode], SystemTime), GameState], keyboardListener: KeyboardListener, screenDimensions: ScreenDimensions, graphicsContext : CanvasRenderingContext2D, clear: () => Unit): Unit = {
    clear()

    val (state, next) = co.run((keyboardListener.pressedKeyCodes, SystemTime.now()))

    if (!state.isGameOver) {
      val rectangles = toRectangles(screenDimensions, state.player, state.pipe)

      rectangles.foreach { case (rectangle, colour) =>
        graphicsContext.fillStyle = colour.value
        graphicsContext.fillRect(rectangle.x, rectangle.y, rectangle.width, rectangle.height)
      }

      dom.setTimeout(() => doFrame(next, keyboardListener, screenDimensions, graphicsContext, clear), 2)
    }
    else {
      graphicsContext.fillStyle = "red"
      graphicsContext.fillRect(0, 0, screenDimensions.width, screenDimensions.height)
    }
  }

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val graphicsContext = canvas.getContext("2d")
      .asInstanceOf[CanvasRenderingContext2D]

    canvas.width = dom.window.innerWidth
    canvas.height = dom.window.innerHeight

    def clear() = {
      graphicsContext.fillStyle = "black"
      graphicsContext.fillRect(0, 0, canvas.width, canvas.height)
    }

    val screenDimensions = ScreenDimensions(dom.window.innerWidth, dom.window.innerHeight)
    val game = Game(screenDimensions)

    val keyboardListener = KeyboardListener(dom.document)

    doFrame(game.game, keyboardListener, screenDimensions, graphicsContext, clear)
  }


}
