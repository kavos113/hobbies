package com.github.kavos113.aiagent.core

import com.intellij.openapi.components.service
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.createFile

class AgentServiceTest : BasePlatformTestCase() {

    private lateinit var agentService: AgentService
    private lateinit var tempDir: Path

    override fun setUp() {
        super.setUp()
        agentService = project.service<AgentService>()
        tempDir = Files.createTempDirectory("agent-service-test")
    }

    override fun tearDown() {
        tempDir.toFile().deleteRecursively()
        super.tearDown()
    }

    fun testUseTool_listFiles() {
        val file1 = tempDir.resolve("file1.txt").createFile()
        val file2 = tempDir.resolve("file2.txt").createFile()
        val subdir = tempDir.resolve("subdir")
        subdir.toFile().mkdir()
        val file3 = subdir.resolve("file3.txt").createFile()

        val content = """
            <list_files>
                <path>${tempDir.toAbsolutePath()}</path>
                <recursive>true</recursive>
            </list_files>
        """.trimIndent()

        val result = agentService.useTool(content)

        println(result)

        val expected = """
            ディレクトリの内容: 
            ${file1.toAbsolutePath()}
            ${file2.toAbsolutePath()}
            ${file3.toAbsolutePath()}
        """.trimIndent()

        assertTrue(result.contains(file1.toString()))
        assertTrue(result.contains(file2.toString()))
        assertTrue(result.contains(file3.toString()))
        assertTrue(result.contains(expected))
    }

    fun testUseTool_executeCommand() {
        val command = "echo Hello, World!"
        val content = """
            <execute_command>
                <command>$command</command>
                <path>${tempDir.toAbsolutePath()}</path>
            </execute_command>
        """.trimIndent()

        val result = agentService.useTool(content)

        val expected = """
            コマンド出力:
            Hello, World!
        """.trimIndent()

        assertTrue(result.contains("Hello, World!"))
        assertTrue(result.contains(expected))
    }

    fun testUseTool_readFile() {
        val file = tempDir.resolve("test.txt").createFile()
        file.toFile().writeText("Hello, World!")

        val content = """
            <read_file>
                <path>${file.toAbsolutePath()}</path>
            </read_file>
        """.trimIndent()

        val result = agentService.useTool(content)

        assertEquals("Hello, World!", result)
    }

    fun testUseTool_writeFile() {
        val file = tempDir.resolve("test.txt").createFile()
        val content = "Hello, World!"

        val writeContent = """
            <write_file>
                <path>${file.toAbsolutePath()}</path>
                <content>$content</content>
            </write_file>
        """.trimIndent()

        val toolResult = agentService.useTool(writeContent)

        val result = file.toFile().readText()

        val expected = """
            ファイルに書き込みました: ${file.toAbsolutePath()}
        """.trimIndent()

        assertEquals(expected, toolResult)
        assertEquals(content, result)
    }

    fun testUseTool_askUser() {
        val content = """
            <ask_user>
                <question>What is your name?</question>
            </ask_user>
        """.trimIndent()

        val result = agentService.useTool(content)

        assertEquals(result, "User response")
    }

    fun testUseTool_taskComplete() {
        val content = """
            <task_complete>
            </task_complete>
        """.trimIndent()

        val result = agentService.useTool(content)

        assertEquals(result, "Task completed successfully")
    }
}