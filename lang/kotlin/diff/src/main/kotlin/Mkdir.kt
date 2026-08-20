import com.anthropic.models.messages.MessageParam
import java.io.File
import java.nio.file.Paths

fun main() {
    val file = File("path/to/file")
    println(file.absolutePath)
    if (!file.exists()) {
        file.mkdirs()
    }

    val dirPath = Paths.get("").toAbsolutePath().toString()
    val files = File(dirPath).listFiles()?.map { if (it.isDirectory) "${it.name}/" else it.name }?.sorted() ?: emptyList()
    files.forEach { println(it) }
}