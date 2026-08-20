import com.github.difflib.DiffUtils
import com.github.difflib.UnifiedDiffUtils
import com.github.difflib.patch.DeltaType
import com.github.difflib.patch.Patch

fun main() {
    val oldStr =
    "hello world\nsome text\nmore text\n\nmoremore text\nhenka nashi\nhenka ari\n";
    val newStr =
    "hello world\nsome more text\nmore text\n\nmoremore text\nhenka nashi\nhenka arisssss\n";

    val oldLines = oldStr.lines()
    val newLines = newStr.lines()
    val patch: Patch<String> = DiffUtils.diff(oldLines, newLines)

    val lines = UnifiedDiffUtils.generateUnifiedDiff(
        "Main.kt",
        "Main.kt",
        oldLines,
        patch,
        3
    )

    val diff = lines.joinToString("\n")
    println(diff)

    val delta = patch.deltas
    val sb = StringBuilder()

    val sortedDeltas = delta.sortedBy { it.source.position }

    var currentPosition = 0
    sortedDeltas.forEach { d ->
        for (i in currentPosition until d.source.position) {
            sb.appendLine(" ${oldLines[i]}")
        }

        when (d.type) {
            DeltaType.CHANGE -> {
                d.source.lines.forEach { sb.appendLine("-$it") }
                d.target.lines.forEach { sb.appendLine("+$it") }
            }
            DeltaType.DELETE -> {
                d.source.lines.forEach { sb.appendLine("-$it") }
            }
            DeltaType.INSERT -> {
                d.target.lines.forEach { sb.appendLine("+$it") }
            }
            DeltaType.EQUAL -> {
                d.source.lines.forEach { sb.appendLine(" $it") }
            }
        }

        currentPosition = d.source.position + d.source.lines.size
    }

    for (i in currentPosition until oldLines.size) {
        sb.appendLine(" ${oldLines[i]}")
    }

    println(sb.toString())
}