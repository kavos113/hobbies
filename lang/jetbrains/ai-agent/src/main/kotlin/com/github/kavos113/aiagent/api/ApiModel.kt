package com.github.kavos113.aiagent.api

import com.anthropic.models.messages.Model
import com.openai.models.ChatModel

sealed class ApiModel {
    data class AnthropicModel(val model: Model): ApiModel()
    data class OpenAiModel(val model: ChatModel): ApiModel()
    data class OpenRouterModel(val model: String): ApiModel()
}