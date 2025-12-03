package com.github.kavos113.aiagent.api

sealed class ApiHandlerOptions {
    data class AnthropicHandlerOptions(
        val apiKey: String,
    ): ApiHandlerOptions()

    data class BedrockHandlerOptions(
        val accessKey: String,
        val secretKey: String,
        val region: String,
    ): ApiHandlerOptions()

    data class OpenAiHandlerOptions(
        val apiKey: String,
    ): ApiHandlerOptions()
    
    data class OpenRouterHandlerOptions(
        val apiKey: String,
    ): ApiHandlerOptions()
}