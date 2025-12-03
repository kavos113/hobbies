package com.github.kavos113.aiagent.api

data class ApiResponse(
    val type: String,
    val id: String,
    val role: ApiMessageParam.Role,
    val content: List<ApiResponseMessage>,
    val inputTokens: Long,
    val outputTokens: Long,
)
