import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.ChatModel
import com.openai.models.chat.completions.ChatCompletionCreateParams

fun main() {
  val client = OpenAIOkHttpClient.builder()
    .apiKey("")
    .baseUrl("https://openrouter.ai/api/v1")
    .build()

  val params = ChatCompletionCreateParams.builder()
    .addUserMessage("Hello, how are you?")
    .model("deepseek/deepseek-r1:free")
    .build()

  val response = client.chat().completions().create(params)
  println(response.choices().first().message().content())
}