package com.github.kavos113.zatsuagent.core

import com.intellij.openapi.components.Service
import com.intellij.openapi.project.Project
import com.openai.client.okhttp.OpenAIOkHttpClient

@Service(Service.Level.PROJECT)
class AiService(project: Project) {
    val client = OpenAIOkHttpClient.builder()
        .apiKey("ollama")
        .baseUrl("http://localhost:11434/v1")
        .build()

    fun sendRequest(prompt: String, onRecieveMessage: (String) -> Unit, onRecieveReasoning: (String) -> Unit) {
        val params = com.openai.models.chat.completions.ChatCompletionCreateParams.builder()
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
}