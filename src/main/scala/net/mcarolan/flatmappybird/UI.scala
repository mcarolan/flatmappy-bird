package net.mcarolan.flatmappybird

import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport

@JSExport
object UI {

  case class Rectangle(colour: String, x: Double, y: Double, width: Double, height: Double)

  def doFrame(co: CoRoutine[SystemTime, Pipe], game: Game, graphicsContext : CanvasRenderingContext2D, clear: () => Unit): Unit = {
    clear()

    val (pipe, next) = co.run(SystemTime.now())

    val rectangles = game.toRectangles(pipe)

    rectangles.foreach { rectangle =>
      graphicsContext.fillStyle = rectangle.colour
      graphicsContext.fillRect(rectangle.x, rectangle.y, rectangle.width, rectangle.height)
    }

    dom.setTimeout(() => doFrame(next, game, graphicsContext, clear), 2)
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

    val game = Game(dom.window.innerWidth, dom.window.innerHeight)

    doFrame(game.pipe, game, graphicsContext, clear)
  }


}
