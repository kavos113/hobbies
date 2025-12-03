import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.ChatModel
import com.openai.models.chat.completions.ChatCompletionCreateParams

fun main() {
  val client = OpenAIOkHttpClient.builder()
    .apiKey("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")
    .baseUrl("https://openrouter.ai/api/v1")
    .build()

  val params = ChatCompletionCreateParams.builder()
    .addUserMessage("Hello, how are you?")
    .model("deepseek/deepseek-r1:free")
    .build()

  val response = client.chat().completions().create(params)
  println(response.choices().first().message().content())
}