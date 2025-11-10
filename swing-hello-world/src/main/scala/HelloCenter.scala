import java.awt.Dimension
import scala.swing.{Frame, MainFrame, SimpleSwingApplication}

object HelloCenter extends SimpleSwingApplication {
  override def top: Frame = new MainFrame {
    title = "Hello, Center!"
    preferredSize = new Dimension(400, 300)
    centerOnScreen()
  }
}
