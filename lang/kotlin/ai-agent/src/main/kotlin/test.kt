import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.chat.completions.ChatCompletionCreateParams
import com.openai.models.responses.ResponseCreateParams

fun main() {
    val client = OpenAIOkHttpClient.builder()
        .apiKey("ollama")
        .baseUrl("http://localhost:11434/v1")
        .build()

    val params = ChatCompletionCreateParams.builder()
        .model("gpt-oss:20b")
        .addUserMessage("こんにちは！")
        .build()

    client.chat().completions().create(params).choices().stream()
        .flatMap { it.message().content().stream() }
        .forEach { println(it) }
}