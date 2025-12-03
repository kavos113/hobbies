package org.example

@JsName("greet")
external fun jsGreet(name: String) : String

@JsName("setOutput")
external fun jsSetOutput(message: String)

fun main() {
    val kotlinName = "Kotlin"
    val greetingFromJs = jsGreet(kotlinName)
    println(greetingFromJs)
    jsSetOutput("message from Kotlin: $greetingFromJs")
}