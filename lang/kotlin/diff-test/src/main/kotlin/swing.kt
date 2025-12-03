import java.awt.Color
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JTextArea
import javax.swing.border.LineBorder
import javax.swing.text.DefaultCaret

fun main() {
    JFrame().apply {
        add(JPanel().apply {
            val longText = "This is very very long text that should be wrapped in a JLabel. " +
                    "This is very very long text that should be wrapped in a JLabel. " +
                    "This is very very long text that should be wrapped in a JLabel. " +
                    "This is very very long text that should be wrapped in a JLabel. " +
                    "This is very very long text that should be wrapped in a JLabel. "
            val textArea = JTextArea(longText).apply {
                lineWrap = true
                wrapStyleWord = true
                background = null
                border = LineBorder(Color.RED)
                isEditable = false
                caret.isVisible = false
                (caret as DefaultCaret).updatePolicy = DefaultCaret.NEVER_UPDATE
                isFocusable = false
            }

//            add(textArea)

        })
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        setSize(300, 200)
        isVisible = true
    }
}