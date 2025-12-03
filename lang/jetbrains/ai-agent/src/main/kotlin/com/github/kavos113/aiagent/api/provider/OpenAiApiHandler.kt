package com.github.kavos113.aiagent.api.provider

import com.github.kavos113.aiagent.api.ApiHandler
import com.github.kavos113.aiagent.api.ApiHandlerOptions
import com.github.kavos113.aiagent.api.ApiMessageParam
import com.github.kavos113.aiagent.api.ApiModel
import com.github.kavos113.aiagent.api.ApiParam
import com.github.kavos113.aiagent.api.ApiResponse
import com.github.kavos113.aiagent.api.ApiResponseMessage
import com.openai.client.OpenAIClient
import com.openai.client.okhttp.OpenAIOkHttpClient
import com.openai.models.responses.EasyInputMessage
import com.openai.models.responses.ResponseCreateParams
import com.openai.models.responses.ResponseInputItem

class OpenAiApiHandler(options: ApiHandlerOptions.OpenAiHandlerOptions): ApiHandler {
    private val client: OpenAIClient = OpenAIOkHttpClient.builder()
        .apiKey(options.apiKey)
        .build()

    override fun sendRequest(param: ApiParam): ApiResponse {
        val model = when(param.model) {
            is ApiModel.AnthropicModel -> throw IllegalArgumentException("Anthropic model not supported for OpenAI API")
            is ApiModel.OpenAiModel -> param.model.model
            is ApiModel.OpenRouterModel -> throw IllegalArgumentException("OpenRouter model not supported for OpenAI API")
        }

        val inputItems = param.messages.map { message ->
            ResponseInputItem.ofEasyInputMessage(EasyInputMessage.builder()
                .role(when(message.role) {
                    ApiMessageParam.Role.USER -> EasyInputMessage.Role.USER
                    ApiMessageParam.Role.ASSISTANT -> EasyInputMessage.Role.ASSISTANT
                })
                .content(message.content)
                .build()
            )
        }

        val params = ResponseCreateParams.builder()
            .inputOfResponse(inputItems)
            .model(model)
            .build()

        val response = client.responses().create(params)
        val message = response.output()[0].asMessage()

        return ApiResponse(
            type = message._type().toString(),
            id = message.id(),
            role = when(message._role().toString()) {
                "user" -> ApiMessageParam.Role.USER
                "assistant" -> ApiMessageParam.Role.ASSISTANT
                else -> throw IllegalArgumentException("Unknown role: ${message._role()}")
            },
            content = message.content().map { content ->
                ApiResponseMessage(
                    type = "text",
                    text = content.asOutputText().text()
                )
            },
            inputTokens = response.usage().get().inputTokens(),
            outputTokens = response.usage().get().outputTokens(),
        )
    }
}