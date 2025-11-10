import scala.swing.{Button, Frame, MainFrame, SimpleSwingApplication}

object HelloWorld extends SimpleSwingApplication {
  override def top: Frame = new MainFrame {
    title = "Hello, World!"
    contents = new Button("Click Me")
  }
}
