package com.github.kavos113.aiagent.api

import com.github.kavos113.aiagent.api.provider.AnthropicApiHandler
import com.github.kavos113.aiagent.api.provider.BedrockApiHandler
import com.github.kavos113.aiagent.api.provider.OpenAiApiHandler
import com.github.kavos113.aiagent.api.provider.OpenRouterApiHandler

interface ApiHandler {
    fun sendRequest(param: ApiParam): ApiResponse

    companion object {
        fun getHandler(options: ApiHandlerOptions): ApiHandler {
            return when(options) {
                is ApiHandlerOptions.AnthropicHandlerOptions -> AnthropicApiHandler(options)
                is ApiHandlerOptions.BedrockHandlerOptions -> BedrockApiHandler(options)
                is ApiHandlerOptions.OpenAiHandlerOptions -> OpenAiApiHandler(options)
                is ApiHandlerOptions.OpenRouterHandlerOptions -> OpenRouterApiHandler(options)
            }
        }
    }
}