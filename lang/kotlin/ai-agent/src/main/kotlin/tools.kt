import com.fasterxml.jackson.annotation.JsonClassDescription
import com.fasterxml.jackson.annotation.JsonPropertyDescription
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Scanner

object Tools {
    @JsonClassDescription("Read specific file and return its content as a string.")
    class ReadFile {
        @JsonPropertyDescription("The absolute path to the file to read.")
        lateinit var filePath: String

        fun execute(): String {
            return readFile(filePath)
        }
    }

    @JsonClassDescription("Write content to a specific file.")
    class WriteFile {
        @JsonPropertyDescription("The absolute path to the file to write.")
        lateinit var filePath: String

        @JsonPropertyDescription("The content to write to the file.")
        lateinit var content: String

        fun execute(): String {
            writeFile(filePath, content)
            return "Content written successfully to file '$filePath'."
        }
    }

    @JsonClassDescription("List all files in a specific directory.")
    class ListFiles {
        @JsonPropertyDescription("The absolute path to the directory to list files from.")
        lateinit var directoryPath: String

        fun execute(): String {
            val files = listFiles(directoryPath)
            if (files.isEmpty()) {
                return "No files found in the directory."
            } else {
                return "Files in directory '$directoryPath':\n" + files.joinToString("\n")
            }
        }
    }

    @JsonClassDescription("Replace text in a specific file.")
    class Replace {
        @JsonPropertyDescription("The absolute path to the file to modify.")
        lateinit var filePath: String

        @JsonPropertyDescription("The text to search for in the file.")
        lateinit var oldText: String

        @JsonPropertyDescription("The text to replace the old text with.")
        lateinit var newText: String

        fun execute(): String {
            val success = replace(filePath, oldText, newText)

            return when {
                success -> "Text replaced successfully in file '$filePath'."
                else -> "Failed to replace text in file '$filePath'."
            }
        }
    }

    @JsonClassDescription("Execute a shell command.")
    class ShellCommand {
        @JsonPropertyDescription("The shell command to execute.")
        lateinit var command: String

        @JsonPropertyDescription("The working directory (absolute path) to execute the command in.")
        lateinit var workingDirectory: String

        fun execute(): String {
            return "Executing command: $command\n" +
                   "Working directory: $workingDirectory\n" +
                    shellCommand(command, workingDirectory)
        }
    }

    @JsonClassDescription("Indicates that the task is complete.")
    class TaskComplete {
        @JsonPropertyDescription("This tool does not require any parameters.")
        @Suppress("unused")
        lateinit var dummy: String

        fun execute() {
            println("タスクが完了しました。")
        }
    }

    fun readFile(filePath: String): String {
        return try {
            Files.readString(Paths.get(filePath))
        } catch (e: Exception) {
            println("Error reading file: $e")
            ""
        }
    }

    fun writeFile(filePath: String, content: String) {
        try {
            Files.writeString(Paths.get(filePath), content)
        } catch (e: Exception) {
            println("Error writing file: $e")
        }
    }

    fun listFiles(directoryPath: String): List<String> {
        return try {
            Files.list(Paths.get(directoryPath))
                .map { it.toString() }
                .toList()
        } catch (e: Exception) {
            println("Error listing files: $e")
            emptyList()
        }
    }

    fun replace(
        filePath: String,
        oldText: String,
        newText: String
    ): Boolean {
        return try {
            val content = readFile(filePath)
            if (content.contains(oldText)) {
                val updatedContent = content.replace(oldText, newText)
                writeFile(filePath, updatedContent)
                true
            } else {
                false
            }
        } catch (e: Exception) {
            println("Error replacing text: $e")
            false
        }
    }

    fun shellCommand(
        command: String,
        workingDirectory: String
    ): String {
        println("このコマンドの実行を許可しますか？: $command")
        Scanner(System.`in`).use { scanner ->
            println("y/n: ")
            val response = scanner.nextLine().trim().lowercase()
            if (response != "y") {
                return "Command execution cancelled by user."
            }
        }

        return try {
            val processBuilder = ProcessBuilder(*command.split(" ").toTypedArray())
            processBuilder.directory(File(workingDirectory))
            val process = processBuilder.start()
            val output = process.inputStream.bufferedReader().readText()
            process.waitFor()
            output
        } catch (e: Exception) {
            println("Error executing shell command: $e")
            ""
        }
    }
}