package extendfunction

class SomeImplement(val id: String) {

    fun printValue(value: Int) {
        println("given value by $id: $value")
    }
}

abstract class SomeClass {
    abstract val imp: SomeImplement
    var counter = 0
}

fun SomeClass.someFunc() {
    imp.printValue(10)
    counter++
}

fun SomeClass.parentFunc(
    content: SomeClass.() -> Unit
) {
    val some = object : SomeClass() {
        override val imp: SomeImplement = SomeImplement("internal")
    }

    some.content()
}

fun SomeClass.useParent() {
    parentFunc {
        someFunc()
    }
}

fun SomeClass.notUseParent() {
    parentFunc {
        imp.printValue(20)
        counter++
    }
}

fun main() {
    val root = object : SomeClass() {
        override val imp: SomeImplement = SomeImplement("root")
    }

    root.useParent()
    root.notUseParent()
    println("counter of root: ${root.counter}")
}