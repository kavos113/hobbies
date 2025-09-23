package com.github.kavos113.zatsuagent.core

import com.intellij.openapi.components.Service
import com.intellij.openapi.project.Project
import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.chat.completions.ChatCompletionCreateParams
import com.openai.models.chat.completions.ChatCompletionMessageFunctionToolCall
import com.openai.models.chat.completions.ChatCompletionToolMessageParam
import java.nio.file.Paths
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

@Service(Service.Level.PROJECT)
class AiService(project: Project) {
    val client = OpenAIOkHttpClient.builder()
        .apiKey("ollama")
        .baseUrl("http://localhost:11434/v1")
        .build()

    // --- 既存チャットストリーミング ---
    fun sendRequest(prompt: String, onRecieveMessage: (String) -> Unit, onRecieveReasoning: (String) -> Unit) {
        val params = ChatCompletionCreateParams.builder()
            .model("gpt-oss:20b")
            .addUserMessage(prompt)
            .build()

        val accumulator = com.openai.helpers.ChatCompletionAccumulator.create()

        client.chat().completions().createStreaming(params).use { streamResponse ->
            streamResponse.stream()
                .peek(accumulator::accumulate)
                .forEach { chunk ->
                    chunk.choices().forEach { choice ->
                        choice.delta().content().ifPresent {
                            onRecieveMessage(it)
                        }
                        choice.delta()._additionalProperties()["reasoning"]?.let { reasoning ->
                            onRecieveReasoning(reasoning.toString())
                        }
                    }
                }
        }
    }

    // --- エージェント実行（従来シグネチャ互換） ---
    fun runAgent(prompt: String) {
        runAgentInternal(
            prompt = prompt,
            onMessage = { println(it) },
            onTool = { println(it) },
            onComplete = { println("Task completed successfully.") }
        )
    }

    // --- UI等から利用するコールバック付きエージェント実行 ---
    fun runAgentAsync(
        prompt: String,
        onMessage: (String) -> Unit,
        onTool: (String) -> Unit,
        onComplete: () -> Unit
    ) {
        Thread { // 簡易: PooledThreadでも良いが依存を避ける
            try {
                runAgentInternal(prompt, onMessage, onTool, onComplete)
            } catch (e: Exception) {
                onMessage("[Agent Error] ${'$'}{e.message}")
            }
        }.start()
    }

    // 実際のループ本体
    private fun runAgentInternal(
        prompt: String,
        onMessage: (String) -> Unit,
        onTool: (String) -> Unit,
        onComplete: () -> Unit
    ) {
        val paramBuilder = createAgentParams(prompt)

        while (true) {
            val response = client.chat().completions().create(paramBuilder.build())
            var taskCompleted = false
            response.choices()
                .asSequence()
                .map { it.message() }
                .onEach { paramBuilder.addMessage(it) }
                .forEach { message ->
                    message.content().ifPresent { content ->
                        if (content.isNotBlank()) onMessage(content)
                    }
                    val toolCalls = message.toolCalls().orElse(emptyList())
                    toolCalls.forEach { toolCall ->
                        val function = toolCall.asFunction()
                        val result = callFunction(function.function())
                        onTool("${'$'}{function.function().name()}: ${'$'}result")

                        if (function.function().name() == "TaskComplete") {
                            taskCompleted = true
                        }

                        // ツール結果を会話にフィードバック
                        paramBuilder.addMessage(
                            ChatCompletionToolMessageParam.builder()
                                .toolCallId(function.id())
                                .contentAsJson(result)
                                .build()
                        )
                    }
                }
            if (taskCompleted) {
                onComplete()
                return
            }
        }
    }

    fun callFunction(function: ChatCompletionMessageFunctionToolCall.Function): Any {
        return when (function.name()) {
            "ReadFile" -> function.arguments(Tools.ReadFile::class.java).execute()
            "WriteFile" -> function.arguments(Tools.WriteFile::class.java).execute()
            "ListFiles" -> function.arguments(Tools.ListFiles::class.java).execute()
            "Replace" -> function.arguments(Tools.Replace::class.java).execute()
            "ShellCommand" -> function.arguments(Tools.ShellCommand::class.java).execute()
            "TaskComplete" -> function.arguments(Tools.TaskComplete::class.java).execute()
            else -> "Unknown function: ${'$'}{function.name()}"
        }
    }

    companion object {
        // テスト容易化: Builder生成を切り出し
        fun createAgentParams(prompt: String): ChatCompletionCreateParams.Builder =
            ChatCompletionCreateParams.builder()
                .model("gpt-oss:20b")
                .addTool(Tools.ReadFile::class.java)
                .addTool(Tools.WriteFile::class.java)
                .addTool(Tools.ListFiles::class.java)
                .addTool(Tools.Replace::class.java)
                .addTool(Tools.ShellCommand::class.java)
                .addTool(Tools.TaskComplete::class.java)
                .addSystemMessage(PROMPT)
                .addUserMessage(prompt)
    }
}