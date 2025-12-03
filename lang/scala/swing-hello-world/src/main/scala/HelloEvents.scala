import java.awt.Dimension
import scala.swing.event.{KeyPressed, MouseClicked, MouseMoved}
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}

object HelloEvents extends SimpleSwingApplication {

  private val panel: Panel = new Panel {
    preferredSize = new Dimension(400, 300)
    focusable = true
  }

  override def top: Frame = new MainFrame {
    title = "Hello, Events!"
    preferredSize = new Dimension(400, 300)
    centerOnScreen()

    contents = panel

    println("Click inside the window or press any key...")

    listenTo(panel.mouse.moves)
    listenTo(panel.mouse.clicks)
    listenTo(panel.keys)

    reactions += {
      case MouseMoved(_, point, _) =>
        println(s"Mouse moved to: $point")
      case MouseClicked(_, point, _, _, _) =>
        println(s"Mouse clicked at: $point")
      case KeyPressed(_, key, _, _) =>
        println(s"Key pressed: $key")
    }
  }
}
