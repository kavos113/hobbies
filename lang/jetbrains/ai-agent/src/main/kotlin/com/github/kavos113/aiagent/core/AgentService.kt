package com.github.kavos113.aiagent.core

import com.intellij.openapi.components.Service
import com.intellij.openapi.diagnostic.thisLogger
import com.intellij.openapi.project.Project
import com.github.kavos113.aiagent.MyBundle
import com.github.kavos113.aiagent.api.ApiHandler
import com.github.kavos113.aiagent.api.ApiHandlerOptions
import com.github.kavos113.aiagent.api.ApiParam
import org.w3c.dom.Element
import javax.xml.parsers.DocumentBuilderFactory

@Service(Service.Level.PROJECT)
class AgentService(project: Project) {

    val apiHandler = ApiHandler.getHandler(
        ApiHandlerOptions.OpenRouterHandlerOptions(
            apiKey = "sss"
        )
    )

    init {
        thisLogger().info(MyBundle.message("projectService", project.name))
        thisLogger().warn("Don't forget to remove all non-needed sample code files with their corresponding registration entries in `plugin.xml`.")
    }

    fun getRandomNumber() = (1..100).random()

    fun createApiRequest(param: ApiParam) {
        val response = apiHandler.sendRequest(param)
        thisLogger().info("API Response: $response")
    }

    fun useTool(content: String): String {
        val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
        val document = builder.parse(content.byteInputStream())

        document.documentElement.normalize()

        val root = document.documentElement
        val tool = Tools.ToolName.fromString(root.tagName)

        val children = root.childNodes
            .let { nodes -> (0 until nodes.length).map { nodes.item(it) } }
            .filterIsInstance<Element>()
            .associate { it.tagName to it.textContent }

        return when (tool) {
            Tools.ToolName.ListFiles -> {
                val notFoundArguments = mutableListOf<String>()
                val path = children["path"] ?: run {
                    notFoundArguments.add("path")
                    null
                }
                val recursive = children["recursive"]?.toBoolean() ?: run {
                    notFoundArguments.add("recursive")
                    null
                }

                if (notFoundArguments.isNotEmpty()) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                if (path == null || recursive == null) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                Tools.listFiles(path, recursive)
            }
            Tools.ToolName.ExecuteCommand -> {
                val notFoundArguments = mutableListOf<String>()
                val command = children["command"] ?: run {
                    notFoundArguments.add("command")
                    null
                }
                val path = children["path"] ?: run {
                    notFoundArguments.add("path")
                    null
                }
                if (notFoundArguments.isNotEmpty()) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }
                if (command == null || path == null) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                Tools.executeCommand(command, path)
            }
            Tools.ToolName.ReadFile -> {
                val notFoundArguments = mutableListOf<String>()
                val path = children["path"] ?: run {
                    notFoundArguments.add("path")
                    null
                }

                if (notFoundArguments.isNotEmpty()) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                if (path == null) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                Tools.readFile(path)
            }
            Tools.ToolName.WriteFile -> {
                val notFoundArguments = mutableListOf<String>()
                val path = children["path"] ?: run {
                    notFoundArguments.add("path")
                    null
                }
                val content = children["content"] ?: run {
                    notFoundArguments.add("content")
                    null
                }

                if (notFoundArguments.isNotEmpty()) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                if (path == null || content == null) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                Tools.writeFile(path, content)
            }
            Tools.ToolName.AskUser -> {
                val notFoundArguments = mutableListOf<String>()
                val question = children["question"] ?: run {
                    notFoundArguments.add("question")
                    null
                }

                if (notFoundArguments.isNotEmpty()) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                if (question == null) {
                    return createIllegalArgumentResponse(tool, notFoundArguments)
                }

                Tools.askUser(question)
            }
            Tools.ToolName.TaskComplete -> {
                Tools.taskComplete()
            }
        }
    }

    private fun createIllegalArgumentResponse(toolName: Tools.ToolName, notFoundArguments: List<String>): String {
        return """
            tool $toolName は以下の引数を必要とします:
            ${notFoundArguments.joinToString(", ")}
        """.trimIndent()
    }
}
