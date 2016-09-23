package net.mcarolan.flatmappybird

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLDocument, HTMLElement, HTMLImageElement}

import scala.scalajs.js.annotation.JSExport

@JSExport
object UI {

  abstract class Image(src: String) {
    def image = {
      val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      element.src = src
      element
    }
  }

  /*
  http://opengameart.org/content/2d-object-pack
   */
  case object PipeTopBackgroundImage extends Image("pipe-top-background.png")
  case object PipeBottomBackgroundImage extends Image("pipe-bottom-background.png")
  case object PipeTopImage extends Image("pipe-top.png")
  case object PipeBottomImage extends Image("pipe-bottom.png")
  // http://opengameart.org/content/fat-bird-sprite-sheets-for-gamedev
  case object BirdImage extends Image("bird.png")

  def draw(graphicsContext : CanvasRenderingContext2D, image: Image, rectangle: Rectangle): Unit = {
    graphicsContext.drawImage(image.image, rectangle.x, rectangle.y, rectangle.width, rectangle.height)
  }

  case class Colour(value: String)

  def toRectangles(screenDimensions: ScreenDimensions, player: Player, pipe: Pipe): List[(Rectangle, Image)] = {
    val pipeBottom = Rectangle(pipe.currentX, pipe.currentGap + Pipe.gapSize, Pipe.width, 80)
    val pipeTop = Rectangle(pipe.currentX, pipe.currentGap - 80, Pipe.width, 80)
    List(
      pipe.topRectangle -> PipeTopBackgroundImage,
      pipe.bottomRectangle(screenDimensions) -> PipeBottomBackgroundImage,
      pipeBottom -> PipeBottomImage,
      player.toRectangle -> BirdImage,
      pipeTop -> PipeTopImage)
  }

  def doFrame(co: CoRoutine[(Set[KeyCode], SystemTime), GameState],
              keyboardListener: KeyboardListener,
              screenDimensions: ScreenDimensions,
              graphicsContext : CanvasRenderingContext2D,
              clear: () => Unit): Unit = {
    clear()

    val (state, next) = co.run((keyboardListener.pressedKeyCodes, SystemTime.now()))

    if (!state.isGameOver) {
      val rectangles = toRectangles(screenDimensions, state.player, state.pipe)

      rectangles.foreach { case (rectangle, image) =>
          draw(graphicsContext, image, rectangle)
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
      graphicsContext.fillStyle = "white"
      graphicsContext.fillRect(0, 0, canvas.width, canvas.height)
    }

    val screenDimensions = ScreenDimensions(dom.window.innerWidth, dom.window.innerHeight)
    val game = Game(screenDimensions)

    val keyboardListener = KeyboardListener(dom.document)

    doFrame(game.game, keyboardListener, screenDimensions, graphicsContext, clear)
  }


}
