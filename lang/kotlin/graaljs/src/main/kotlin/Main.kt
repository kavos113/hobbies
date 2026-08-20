package org.example

import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Source
import org.graalvm.polyglot.Value
import org.graalvm.polyglot.io.IOAccess
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets

const val JS_CODE = """
    function hello(param) {
        console.log("this is JS code");
        return "Hello " + param + " from JS!";
    }
    
    function add(a, b) {
        return a + b;
    }
"""

fun jsHello(context: Context): Value? {
    val helloFunction = context.getBindings("js").getMember("hello")

    return helloFunction.execute("Kotlin")
}

fun jsAdd(context: Context, a: Int, b: Int): Value? {
    val addFunction = context.getBindings("js").getMember("add")

    return addFunction.execute(a, b)
}

fun main() {
//    val tsCode = object {}.javaClass.getResource("/bundle.js")
//        ?: throw IllegalStateException("Failed to read TypeScript code from bundle.js")
//
//    Context.newBuilder("js")
//        .option("js.esm-eval-returns-exports", "true")
//        .build()
//        .use{ context ->
//            val source = Source.newBuilder("js", tsCode)
//                .mimeType("application/javascript+module")
//                .build()
//            val exports = context.eval(source)
//
//            val processUsersFunction = exports.getMember("processUsers")
//            val usersArray = context.eval("js", """
//                [
//                  { name: 'Alice', age: 30 },
//                  { name: 'Bob', age: 25 },
//                  { name: 'Charlie', age: 35 },
//                ]
//            """)
//            val processedNamesResult = processUsersFunction.execute(usersArray)
//
//            if (processedNamesResult.hasArrayElements()) {
//                val processedNames = (0 until processedNamesResult.arraySize).map {
//                    processedNamesResult.getArrayElement(it).asString()
//                }
//                println("[Kotlin] Processed Names from TypeScript/Lodash (esbuild.js): ${processedNames.joinToString(", ")}")
//            }
//        }

    val clineCodeUrl = object {}.javaClass.getResource("/time.js")
        ?: throw IllegalStateException("Failed to read Cline code from extensions.js")

    clineCodeUrl.openStream().use { inputStream ->
        val clineCode = InputStreamReader(inputStream, StandardCharsets.UTF_8).readText()

        Context.newBuilder("js")
            .allowIO(IOAccess.ALL)
            .option("js.esm-eval-returns-exports", "true")
            .build()
            .use { context ->
                val source = Source.newBuilder("js", clineCode, "time.js")
                    .mimeType("application/javascript+module")
                    .build()
                val exports = context.eval(source)

                val secondsToMs = exports.getMember("secondsToMs")
                val result = secondsToMs.execute(5)

                println("[Kotlin] Result from secondsToMs from Cline: $result")
            }
    }

//    val options = mapOf("js.commonjs-require" to "true")
//
//    val streamCodeUrl = object {}.javaClass.getResource("/stream.js")
//        ?: throw IllegalStateException("Failed to read Cline code from extensions.js")
//
//    streamCodeUrl.openStream().use { inputStream ->
//        val clineCode = InputStreamReader(inputStream, StandardCharsets.UTF_8).readText()
//
//        Context.newBuilder("js")
//            .allowExperimentalOptions(true)
//            .allowIO(IOAccess.ALL)
//            .options(options)
//            .build()
//            .use { context ->
//                val source = Source.newBuilder("js", clineCode, "stream.js")
//                    .mimeType("application/javascript+module")
//                    .build()
//                context.eval(source)
//            }
//    }

}