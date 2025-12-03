package com.github.kavos113.zatsuagent.ui

import com.intellij.openapi.diagnostic.thisLogger
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow
import com.intellij.openapi.wm.ToolWindowFactory
import com.intellij.ui.components.JBPanel
import com.intellij.ui.content.ContentFactory
import java.awt.BorderLayout

class ChatWindowFactory : ToolWindowFactory {

    init {
        thisLogger().warn("Sample code removed: providing Chat window UI.")
    }

    override fun createToolWindowContent(project: Project, toolWindow: ToolWindow) {
        val chatWindow = ChatWindow(project)
        val content = ContentFactory.getInstance().createContent(chatWindow.getContent(), null, false)
        toolWindow.contentManager.addContent(content)
    }

    override fun shouldBeAvailable(project: Project) = true

    class ChatWindow(val project: Project) {
        fun getContent() = JBPanel<JBPanel<*>>(BorderLayout()).apply {
            add(ChatPanel(project), BorderLayout.CENTER)
        }
    }
}
