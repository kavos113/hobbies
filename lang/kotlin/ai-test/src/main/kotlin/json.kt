import com.openai.core.JsonObject
import com.openai.core.JsonValue

fun main() {
  val v = JsonValue.from("{key: 'value'}")
  val v2 = JsonObject.of(
    mapOf(
      "key" to JsonValue.from("value")
    )
  )
  println(v)
  println(v2)
}