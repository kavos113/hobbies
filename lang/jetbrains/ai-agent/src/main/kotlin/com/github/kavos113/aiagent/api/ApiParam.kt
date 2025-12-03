package com.github.kavos113.aiagent.api

data class ApiParam(
    val model: ApiModel,
    val systemPrompt: String,
    val messages: List<ApiMessageParam>,
)