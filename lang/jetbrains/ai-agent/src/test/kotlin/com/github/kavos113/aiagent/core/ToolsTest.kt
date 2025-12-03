package com.github.kavos113.aiagent.core

import com.intellij.testFramework.fixtures.BasePlatformTestCase
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.createFile
import kotlin.io.path.writeText

class ToolsTest : BasePlatformTestCase() {

    private lateinit var tempDir: Path

    override fun setUp() {
        tempDir = Files.createTempDirectory("tools-test")
    }

    override fun tearDown() {
        tempDir.toFile().deleteRecursively()
    }

    fun testListFiles_notRecursively() {
        val file1 = tempDir.resolve("file1.txt").createFile()
        val file2 = tempDir.resolve("file2.txt").createFile()

        val result = Tools.listFiles(tempDir.toString(), false)

        assertTrue(result.contains(file1.toString()))
        assertTrue(result.contains(file2.toString()))
    }

    fun testListFiles_recursively() {
        val subDir = tempDir.resolve("subdir")
        subDir.toFile().mkdir()
        val file1 = subDir.resolve("file1.txt").createFile()
        val file2 = tempDir.resolve("file2.txt").createFile()

        val result = Tools.listFiles(tempDir.toString(), true)

        assertTrue(result.contains(file1.toString()))
        assertTrue(result.contains(file2.toString()))
    }

    fun testExecuteCommand() {
        val command = "echo Hello, World!"
        val result = Tools.executeCommand(command, tempDir.toString())

        assertTrue(result.contains("Hello, World!"))
    }

    fun testReadFile() {
        val file = tempDir.resolve("test.txt").createFile()
        file.writeText("Hello, World!")

        val result = Tools.readFile(file.toString())
        assertEquals("Hello, World!", result)
    }

    fun testWriteFile() {
        val file = tempDir.resolve("test.txt").createFile()
        val content = "Hello, World!"
        Tools.writeFile(file.toString(), content)

        val result = Tools.readFile(file.toString())
        assertEquals(content, result)
    }
}