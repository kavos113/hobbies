package com.github.kavos113.zatsuagent.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.dsl.builder.AlignX
import com.intellij.ui.dsl.builder.panel
import java.awt.BorderLayout
import java.awt.Font
import javax.swing.JPanel

class ChatMessage(var chat: Chat, private val project: Project) {
    private val container = JPanel(BorderLayout())

    val panel: JPanel = container.apply {
        add(buildContent(), BorderLayout.CENTER)
    }

    private fun buildContent(): JPanel = panel {
        row {
            label(chat.name).applyToComponent {
                font = font.deriveFont(Font.BOLD, 16f)
            }
        }
        row {
            cell(MarkdownPanel(chat.message, project))
                .align(AlignX.FILL)
                .resizableColumn()
        }
    }

    fun setMessage(newMessage: String) {
        chat = chat.copy(message = newMessage)
        container.removeAll()
        container.add(buildContent(), BorderLayout.CENTER)
        container.revalidate()
        container.repaint()
    }
}