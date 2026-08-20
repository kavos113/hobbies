package debounce

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.FlowPreview
import kotlinx.coroutines.currentCoroutineContext
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.debounce
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.flowOn
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.isActive
import kotlinx.coroutines.runBlocking
import java.util.Scanner
import kotlin.time.Duration.Companion.milliseconds

fun lineInputFlow(): Flow<String> = flow {
    val scanner = Scanner(System.`in`)

    while (currentCoroutineContext().isActive && scanner.hasNextLine()) {
        val line = scanner.nextLine()
        emit(line)
    }
}.flowOn(Dispatchers.IO)

@OptIn(FlowPreview::class)
fun main() = runBlocking {
    println("----start-----")

    val lines = mutableListOf<String>()

    lineInputFlow()
        .onEach { lines.add(it) }
        .debounce(1000.milliseconds)
        .collect {
            val chunk = lines.toList()
            lines.clear()

            println("[result]; $chunk")
        }
}