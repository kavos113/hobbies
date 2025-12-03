package com.github.kavos113.aiagent.api.provider

import com.anthropic.client.AnthropicClient
import com.anthropic.client.okhttp.AnthropicOkHttpClient
import com.anthropic.models.messages.MessageCreateParams
import com.anthropic.models.messages.MessageParam
import com.github.kavos113.aiagent.api.ApiHandler
import com.github.kavos113.aiagent.api.ApiHandlerOptions
import com.github.kavos113.aiagent.api.ApiMessageParam
import com.github.kavos113.aiagent.api.ApiModel
import com.github.kavos113.aiagent.api.ApiParam
import com.github.kavos113.aiagent.api.ApiResponse
import com.github.kavos113.aiagent.api.ApiResponseMessage

class AnthropicApiHandler(options: ApiHandlerOptions.AnthropicHandlerOptions): ApiHandler {
    private val client: AnthropicClient = AnthropicOkHttpClient.builder()
        .apiKey(options.apiKey)
        .build()

    override fun sendRequest(param: ApiParam): ApiResponse {
        val model = when (param.model) {
            is ApiModel.AnthropicModel -> param.model.model
            is ApiModel.OpenAiModel -> throw IllegalArgumentException("OpenAI model not supported for Anthropic API")
            is ApiModel.OpenRouterModel -> throw IllegalArgumentException("OpenRouter model not supported for Anthropic API")
        }

        val params = MessageCreateParams.builder()
            .model(model)
            .maxTokens(4096L)
            .system(param.systemPrompt)
            .messages(param.messages.map { message ->
                MessageParam.builder()
                    .role(when(message.role) {
                        ApiMessageParam.Role.USER -> MessageParam.Role.USER
                        ApiMessageParam.Role.ASSISTANT -> MessageParam.Role.ASSISTANT
                    })
                    .content(message.content)
                    .build()
            })
            .build()

        val response = client.messages().create(params)

        return ApiResponse(
            type = response._type().toString(),
            id = response.id(),
            role = when(response._role().toString()) {
                "user" -> ApiMessageParam.Role.USER
                "assistant" -> ApiMessageParam.Role.ASSISTANT
                else -> throw IllegalArgumentException("Unknown role: ${response._role()}")
            },
            content = response.content().map { content ->
                ApiResponseMessage(
                    type = "text",
                    text = content.asText().text()
                )
            },
            inputTokens = response.usage().inputTokens(),
            outputTokens = response.usage().outputTokens(),
        )
    }
}