package net.mcarolan.flatmappybird

import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport

@JSExport
object UI {

  case class Rectangle(colour: String, x: Double, y: Double, width: Double, height: Double)

  def toRectangles(screenDimensions: ScreenDimensions, p: Pipe): Set[Rectangle] =
    Set(
      Rectangle("green", p.currentX.value, 0.0, Pipe.width, p.currentGap.value),
      Rectangle("green", p.currentX.value, p.currentGap.value + Pipe.gapSize, Pipe.width, screenDimensions.height - Pipe.gapSize - p.currentGap.value))

  def doFrame(co: CoRoutine[SystemTime, Pipe], screenDimensions: ScreenDimensions, graphicsContext : CanvasRenderingContext2D, clear: () => Unit): Unit = {
    clear()

    val (pipe, next) = co.run(SystemTime.now())

    val rectangles = toRectangles(screenDimensions, pipe)

    rectangles.foreach { rectangle =>
      graphicsContext.fillStyle = rectangle.colour
      graphicsContext.fillRect(rectangle.x, rectangle.y, rectangle.width, rectangle.height)
    }

    dom.setTimeout(() => doFrame(next, screenDimensions, graphicsContext, clear), 2)
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

    doFrame(game.pipe, screenDimensions, graphicsContext, clear)
  }


}
