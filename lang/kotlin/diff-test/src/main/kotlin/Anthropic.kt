import com.anthropic.core.JsonValue
import com.anthropic.core.jsonMapper
import com.anthropic.models.messages.ContentBlock
import com.anthropic.models.messages.Message
import com.anthropic.models.messages.MessageParam
import com.anthropic.models.messages.Model
import com.anthropic.models.messages.TextBlock
import com.anthropic.models.messages.ToolUseBlock
import com.anthropic.models.messages.Usage
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue

fun main() {
    val param: MessageParam = MessageParam.builder()
        .content("content")
        .role(MessageParam.Role.USER)
        .build()

    println(listOf(param, param))

    val json = jacksonObjectMapper().writeValueAsString(listOf(param, param))
    println(json)

    val param2 = jacksonObjectMapper().readValue<List<MessageParam>>(json)
    println(param2)

    val message = Message.builder()
        .content(listOf(
            ContentBlock.ofText(
                TextBlock.builder()
                    .text("Hello, how are you?")
                    .citations(listOf())
                    .build()
            ),
        ))
        .id("")
        .model(Model.CLAUDE_3_7_SONNET_LATEST)
        .stopReason(Message.StopReason.END_TURN)
        .stopSequence("")
        .usage(
            Usage.builder()
                .inputTokens(20)
                .outputTokens(10)
                .cacheReadInputTokens(0)
                .cacheCreationInputTokens(0)
                .build()
        )
        .build()

    val messageTU = Message.builder()
        .content(listOf(
            ContentBlock.ofText(
                TextBlock.builder()
                    .text("Hello, how are you?")
                    .citations(listOf())
                    .build()
            ),
            ContentBlock.ofToolUse(
                ToolUseBlock.builder()
                .name("execute_command")
                .id("aaaa")
                .input(JsonValue.from("{\"command\": \"git status\"}"))
                .build()
            )
        ))
        .id("")
        .model(Model.CLAUDE_3_7_SONNET_LATEST)
        .stopReason(Message.StopReason.END_TURN)
        .stopSequence("")
        .usage(
            Usage.builder()
                .inputTokens(20)
                .outputTokens(10)
                .cacheReadInputTokens(0)
                .cacheCreationInputTokens(0)
                .build()
        )
        .build()

    val contentBlock = ContentBlock.ofToolUse(
        ToolUseBlock.builder()
            .name("execute_command")
            .id("aaaa")
            .input(JsonValue.fromJsonNode(ObjectMapper().readTree("{\"command\": \"git status\"}")))
            .build()
    )

    val toolInput = contentBlock.toolUse().get()._input().convert(ToolInput::class.java)
}

data class ToolInput(
    val path: String? = null,
    val content: String? = null,
    val command: String? = null,
    val question: String? = null,
    val result: String? = null,
)
