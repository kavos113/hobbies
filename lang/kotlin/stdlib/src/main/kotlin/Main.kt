class SomeClass {
    var intvar = 0

    val handler = {
        println("int var: $intvar")
    }
}

fun main() {
    val s = SomeClass()

    s.handler()
    s.intvar = 5
    s.handler()
}