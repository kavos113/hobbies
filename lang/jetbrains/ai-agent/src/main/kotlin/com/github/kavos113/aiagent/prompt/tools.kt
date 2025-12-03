package com.github.kavos113.aiagent.prompt

import com.github.kavos113.aiagent.api.ApiTool
import com.github.kavos113.aiagent.api.ApiToolPropertyInfo
import com.openai.core.JsonObject
import com.openai.core.JsonString

val TOOLS = listOf(
    ApiTool(
        name = "shell",
        description = "Runs a shell command, and returns the output.",
        parameters = listOf(
            ApiToolPropertyInfo(
                name = "command",
                type = "array",
                description = "The command to run.",
                required = true,
                other = mapOf(
                    "items" to JsonObject.of(
                        "type" to JsonString.of("string")
                    )
                )
            )
        )
    ),
    ApiTool(
        name = "patch"
    )
)