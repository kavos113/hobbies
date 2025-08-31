package com.github.kavos113.zatsuagent.ui

import com.github.kavos113.zatsuagent.core.AiService
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.service
import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.dsl.builder.AlignX
import com.intellij.ui.dsl.builder.panel
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Insets
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JTextArea
import javax.swing.SwingUtilities

class ChatPanel(val project: Project) : JPanel(BorderLayout()) {
    private val messages: MutableList<Chat> = SAMPLE_MESSAGES
    private val aiService = project.service<AiService>()

    // Keep UI components per message to update incrementally without re-rendering all
    private val messageUis: MutableList<ChatMessage> = mutableListOf()

    private val listPanel = JPanel().apply {
        layout = GridBagLayout()
    }

    // Track next insertion row for GridBagLayout and keep a bottom glue to consume extra space
    private var nextRow: Int = 0
    private val bottomGlue = JPanel()

    private val inputArea = JTextArea(3, 40).apply {
        lineWrap = true
        wrapStyleWord = true
    }

    private val sendButton = JButton("Send").apply {
        addActionListener {
            val text = inputArea.text.trim()
            if (text.isEmpty()) return@addActionListener
            inputArea.text = ""
            appendUserMessage(text)
            requestAiResponse(text)
        }
    }

    init {
        // Initialize existing messages once
        messages.forEach { addMessageComponent(it) }

        val scrollPane = JBScrollPane(listPanel).apply {
            preferredSize = Dimension(400, 300)
            verticalScrollBar.unitIncrement = 16
            horizontalScrollBarPolicy = javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER
        }
        add(scrollPane, BorderLayout.CENTER)

        val inputPanel = panel {
            row {
                cell(inputArea)
                    .align(AlignX.FILL)
            }
            row {
                cell(sendButton)
                    .align(AlignX.RIGHT)
            }
        }
        add(inputPanel, BorderLayout.SOUTH)

        SwingUtilities.invokeLater { scrollToBottom(scrollPane) }
    }

    private fun addMessageComponent(chat: Chat) {
        val ui = ChatMessage(chat, project)
        messageUis.add(ui)

        // Remove existing bottom glue before adding the new message
        if (bottomGlue.parent === listPanel) {
            listPanel.remove(bottomGlue)
        }

        val c = GridBagConstraints().apply {
            gridx = 0
            gridy = nextRow
            weightx = 1.0
            weighty = 0.0
            fill = GridBagConstraints.HORIZONTAL
            anchor = GridBagConstraints.FIRST_LINE_START
            insets = Insets(4, 0, 4, 0) // vertical spacing between messages
        }
        listPanel.add(ui.panel, c)
        nextRow++

        // Re-add bottom glue to push content to the top and keep whitespace at the bottom
        val glueConstraints = GridBagConstraints().apply {
            gridx = 0
            gridy = nextRow
            weightx = 1.0
            weighty = 1.0
            fill = GridBagConstraints.BOTH
        }
        listPanel.add(bottomGlue, glueConstraints)

        listPanel.revalidate()
        listPanel.repaint()
    }

    private fun scrollToBottom(scrollPane: JBScrollPane) {
        val vertical = scrollPane.verticalScrollBar
        vertical.value = vertical.maximum
    }

    private fun appendUserMessage(text: String) {
        val chat = Chat(name = "User", message = text)
        messages.add(chat)
        addMessageComponent(chat)
        findScrollPane()?.let { scrollToBottom(it) }
    }

    private fun appendAiMessage(text: String) {
        val chat = Chat(name = "AI", message = text)
        messages.add(chat)
        addMessageComponent(chat)
        findScrollPane()?.let { scrollToBottom(it) }
    }

    private fun findScrollPane(): JBScrollPane? =
        (components.firstOrNull { it is JBScrollPane } as? JBScrollPane)

    private fun appendAiPlaceholder(): Int {
        val index = messages.size
        val chat = Chat(name = "AI", message = "")
        messages.add(chat)
        addMessageComponent(chat)
        return index
    }

    private fun appendToAiMessage(index: Int, delta: String) {
        if (index in messages.indices && index in messageUis.indices) {
            val current = messages[index]
            val updated = current.copy(message = current.message + delta)
            messages[index] = updated
            // Update only the affected UI component
            messageUis[index].setMessage(updated.message)
        } else {
            // Fallback: append as a new AI message
            appendAiMessage(delta)
        }
    }

    private fun requestAiResponse(userText: String) {
        // Add a placeholder AI message to stream into
        val aiIndex = appendAiPlaceholder()
        val scrollPane = findScrollPane()

        ApplicationManager.getApplication().executeOnPooledThread {
            try {
                aiService.sendRequest(
                    prompt = userText,
                    onRecieveMessage = { delta ->
                        SwingUtilities.invokeLater {
                            appendToAiMessage(aiIndex, delta)
                            scrollPane?.let { scrollToBottom(it) }
                        }
                    },
                    onRecieveReasoning = { _ ->
                        // For minimal change, ignore reasoning stream for now.
                        // Could be surfaced in a separate message or tooltip later.
                    }
                )
            } catch (t: Throwable) {
                val err = t.message ?: "Unknown error"
                SwingUtilities.invokeLater {
                    appendToAiMessage(aiIndex, "\n\n[Error] $err")
                }
            }
        }
    }
}