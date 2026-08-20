import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.chat.completions.ChatCompletionCreateParams
import com.openai.models.chat.completions.ChatCompletionMessageFunctionToolCall
import com.openai.models.chat.completions.ChatCompletionToolMessageParam
import java.nio.file.Paths
import java.util.logging.Logger
import kotlin.io.path.absolutePathString

val PROMPT = """
    あなたは熟練したソフトウェアエンジニアです。さまざまなプログラミング言語やフレームワークの知識を活用して，タスクを完了させてください。
    
    # スタイル
    - **フォーマット**: GitHubのMarkdownスタイルを使用してください。
    - **対応不可能な場合**: 正当化せず，簡潔にその旨を伝えてください。
    
    # ツール使用時のガイドライン
    - **ファイルパス**: ツールを使用する際は、絶対パスを指定してください。なお，このプロジェクトのルートディレクトリは${Paths.get("src/agent").absolutePathString()}です。
    - **ShellCommandでのユーザーの確認**: ShellCommandツールの実行にはユーザーの確認が必要です。ユーザーによってキャンセルされた場合には，その選択を尊重し，再度コマンド実行を試みず，別の方法を検討してください。
    - **タスク完了**: 与えられたタスクが完了したら、必ずTaskCompleteツールを使用してタスク完了を報告してください。これにより、エージェントは次のタスクに進むことができます。
""".trimIndent()

fun callFunction(function: ChatCompletionMessageFunctionToolCall.Function): Any {
    return when (function.name()) {
        "ReadFile" -> function.arguments(Tools.ReadFile::class.java).execute()
        "WriteFile" -> function.arguments(Tools.WriteFile::class.java).execute()
        "ListFiles" -> function.arguments(Tools.ListFiles::class.java).execute()
        "Replace" -> function.arguments(Tools.Replace::class.java).execute()
        "ShellCommand" -> function.arguments(Tools.ShellCommand::class.java).execute()
        "TaskComplete" -> function.arguments(Tools.TaskComplete::class.java).execute()
        else -> "Unknown function: ${function.name()}"
    }
}

fun main() {
//    val logger = Logger.getLogger("OpenAIClient")

    println("What is your task?")
    val userInput = readlnOrNull() ?: return
//    val userInput = "JavaでHello Worldを出力するプログラムを作成してください。"

    val client = OpenAIOkHttpClient.builder()
        .apiKey("ollama")
        .baseUrl("http://localhost:11434/v1")
        .build()

    val paramBuilder = ChatCompletionCreateParams.builder()
        .model("gpt-oss:20b")
        .addTool(Tools.ReadFile::class.java)
        .addTool(Tools.WriteFile::class.java)
        .addTool(Tools.ListFiles::class.java)
        .addTool(Tools.Replace::class.java)
        .addTool(Tools.ShellCommand::class.java)
        .addTool(Tools.TaskComplete::class.java)
        .addSystemMessage(PROMPT)
        .addUserMessage(userInput)

    while (true) {
//        val forlog = paramBuilder.build()
//        logger.info("Sending request with parameters: ${forlog.model()}, ${forlog.messages()}, ${forlog.tools()}")

        client.chat().completions().create(paramBuilder.build()).choices()
            .asSequence()
            .map { it.message() }
            .onEach { paramBuilder.addMessage(it) }
            .flatMap { message ->
//                logger.info ("Received message: $message")

                message.content().ifPresent { println(it) }
                message.toolCalls().orElse(emptyList())
            }
            .forEach { toolCall ->
                val function = toolCall.asFunction()

//                logger.info("Processing tool call: ${function.id()} - ${function.function().name()}")
                val result = callFunction(function.function())
                println("Tool call result: $result")

                if (function.function().name() == "TaskComplete") {
                    println("Task completed successfully.")
                    return
                }

                paramBuilder.addMessage(
                    ChatCompletionToolMessageParam.builder()
                        .toolCallId(function.id())
                        .contentAsJson(result)
                        .build()
                )
            }
    }
}