package com.github.kavos113.zatsuagent.ui

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.intellij.util.ui.UIUtil
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import javax.swing.BoxLayout
import javax.swing.JEditorPane
import javax.swing.JPanel
import javax.swing.text.html.HTMLEditorKit

const val HTML_SAMPLE = """<html>
    <body>
        <h1>Hi, I am a software engineer. I can help you with your code.</h1>
        <h2>Overview</h2>
        <ul>
            <li>I can help you with your code.</li>
            <li>I can help you with your code.</li>
            <li>I can help you with your code.</li>
        </ul>
        <h2>Code</h2>
        <pre><code>def hello_world():
    print("Hello, world!")
hello_world()
</code></pre>
    </body>
"""

class MarkdownPanel(
    content: String,
    project: Project
) : JPanel() {

    init {
        layout = BoxLayout(this, BoxLayout.Y_AXIS)
        // Allow this panel to shrink to the viewport width and not force horizontal scrolling
        maximumSize = java.awt.Dimension(Int.MAX_VALUE, Int.MAX_VALUE)

        val parser = Parser.builder().build()
        val renderer = HtmlRenderer.builder().build()

        val contents = content.split("```")

        contents.forEachIndexed { i, s ->
            if (s.trim().isEmpty()) {
                return@forEachIndexed
            }
            if (i % 2 == 0) {
                val htmlContent = renderer.render(parser.parse(s))
                val editorPane = createEditorPane(htmlContent)

                add(editorPane)
            } else {
                ApplicationManager.getApplication().invokeLater {
                    val lang = s.substringBefore("\n").trim()
                    val codeContent = s.substring(lang.length).trim()
                    val codeBlock = CodeBlock(
                        code = codeContent,
                        diff = null,
                        path = "code.${getExtFromLanguage(lang)}",
                        isExpanded = true,
                        project = project
                    )

                    codeBlock.size = codeBlock.preferredSize
                    add(codeBlock)

                    revalidate()
                    repaint()
                }
            }
        }
    }

    private fun createEditorPane(content: String): JEditorPane {
        val editorPane = object : JEditorPane("text/html", "") {
            override fun getScrollableTracksViewportWidth(): Boolean = true
        }

        val kit = editorPane.editorKit as HTMLEditorKit
        val styleSheet = kit.styleSheet
        styleSheet.addRule(GENERAL_CHAT_STYLE_SHEET)

        editorPane.text = "<html><body>$content</body></html>"
        editorPane.isEditable = false
        editorPane.background = null
        editorPane.isOpaque = false
        editorPane.font = UIUtil.getLabelFont()
        // Ensure the HTML view wraps within available width and doesn't force horizontal growth
        editorPane.putClientProperty("JEditorPane.honorDisplayProperties", true)
        editorPane.maximumSize = java.awt.Dimension(Int.MAX_VALUE, Int.MAX_VALUE)

        return editorPane
    }

    private fun getExtFromLanguage(language: String): String {
        return when (language) {
            "batch" -> "bat"
            "csharp" -> "cs"
            "dockerfile" -> "Dockerfile"
            "haskell" -> "hs"
            "javascript" -> "js"
            "kotlin" -> "kt"
            "latex" -> "tex"
            "markdown" -> "md"
            "perl" -> "pl"
            "ruby" -> "rb"
            "rust" -> "rs"
            "typescript" -> "ts"
            "python" -> "py"
            else -> language
        }
    }
}
