package com.github.kavos113.aiagent.api

data class ApiMessageParam(
    val role: Role,
    val content: String,
) {
    enum class Role {
        USER,
        ASSISTANT
    }
}