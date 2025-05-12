package com.github.kavos113.aiagent.core

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessAdapter
import com.intellij.execution.process.ProcessEvent
import com.intellij.openapi.util.Key
import java.io.File
import java.util.concurrent.CountDownLatch

object Tools {
    enum class ToolName {
        ListFiles,
        ExecuteCommand,
        ReadFile,
        WriteFile,
        AskUser,
        TaskComplete;

        companion object {
            fun fromString(name: String): ToolName {
                return when(name) {
                    "list_files" -> ListFiles
                    "execute_command" -> ExecuteCommand
                    "read_file" -> ReadFile
                    "write_file" -> WriteFile
                    "ask_user" -> AskUser
                    "task_complete" -> TaskComplete
                    else -> throw IllegalArgumentException("Unknown tool name: $name")
                }
            }
        }
    }

    fun listFiles(path: String, recursive: Boolean): String {
        val sb = StringBuilder()

        val dir = File(path)
        if (!dir.exists()) {
            return "ディレクトリが存在しません。"
        }

        if (!dir.isDirectory) {
            return "与えられたパスはディレクトリではありません。"
        }

        if (recursive) {
            dir.walk().forEach {
                if (!it.isDirectory) {
                    sb.append(it.absolutePath).append("\n")
                }
            }
        } else {
            dir.listFiles()?.forEach {
                if (!it.isDirectory) {
                    sb.append(it.absolutePath).append("\n")
                }
            }
        }

        return "ディレクトリの内容:\n$sb"
    }

    fun executeCommand(command: String, path: String): String {
        val sb = StringBuilder()

        val commandParts = if (System.getProperty("os.name").lowercase().contains("win")) {
            "cmd /c $command".replace("\\", "\\\\")
        } else {
            command
        }.split("\\s+".toRegex())

        val commandLine = GeneralCommandLine(commandParts)
        commandLine.setWorkDirectory(path)
        commandLine.charset = Charsets.UTF_8

        val latch = CountDownLatch(1)

        val processHandler = OSProcessHandler(commandLine)
        processHandler.addProcessListener(object : ProcessAdapter() {
            override fun onTextAvailable(event: ProcessEvent, outputType: Key<*>) {
                sb.append(event.text)
//                println(event.text)
            }

            override fun processTerminated(event: ProcessEvent) {
                latch.countDown()
            }
        })

        processHandler.startNotify()
        latch.await()

        return "コマンド出力:\n$sb"
    }

    fun readFile(path: String): String {
        val file = File(path)
        if (!file.exists()) {
            return "ファイルが存在しません。"
        }

        if (!file.isFile) {
            return "与えられたパスはファイルではありません。"
        }

        return file.readText()
    }

    fun writeFile(path: String, content: String): String {
        val file = File(path)

        if (!file.exists()) {
            return "ファイルが存在しません。"
        }

        if (!file.isFile) {
            return "与えられたパスはファイルではありません。"
        }

        file.writeText(content)
        return "ファイルに書き込みました: $path"
    }

    fun askUser(question: String): String {
        println(question)
        return "User response"
    }

    fun taskComplete(): String {
        return "Task completed successfully"
    }
}