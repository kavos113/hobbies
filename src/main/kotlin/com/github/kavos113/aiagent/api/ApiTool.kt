package com.github.kavos113.aiagent.api

data class ApiTool(
    val name: String,
    val description: String,
    val parameters: List<ApiToolPropertyInfo>,
)