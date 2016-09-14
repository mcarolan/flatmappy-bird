package net.mcarolan.flatmappybird

import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLDocument

case class KeyCode(i: Int)
object KeyCode {
  val space = KeyCode(32)
}

case class KeyboardListener(document: HTMLDocument) {
  var pressedKeyCodes: Set[KeyCode] = Set.empty

  document.onkeydown = { ke: KeyboardEvent =>
    pressedKeyCodes = pressedKeyCodes + KeyCode(ke.keyCode)
  }

  document.onkeyup = { ke: KeyboardEvent =>
    pressedKeyCodes = pressedKeyCodes - KeyCode(ke.keyCode)
  }

  def isPressed(keyCode: KeyCode) =
    pressedKeyCodes contains keyCode
}