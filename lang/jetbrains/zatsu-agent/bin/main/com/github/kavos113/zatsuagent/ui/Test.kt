package com.github.kavos113.zatsuagent.ui

val SAMPLE_MESSAGES = mutableListOf(
    Chat(
        "User",
        "Hello, how are you?"
    ),
    Chat(
        "AI",
        "I'm just a program, but I'm here to help you with your questions!"
    ),
    Chat(
        "User",
        "Please explain this code"
    ),
    Chat(
        "AI",
        """
            # Core Concepts
            - **Chat**: Represents a conversation between a user and an AI.
            - **name**: The name of the participant in the chat (e.g., User, AI).
            - **message**: The content of the message sent by the participant.
            
            ## Example Usage
            ```kotlin
            val chat = Chat(
                name = "User",
                message = "Hello, how are you?"
            )
            ```
            This creates a chat instance where the user greets the AI.
        """.trimIndent()
    ),
)

val SAMPLE_CHAT = Chat(
    "User",
    "Hello, how are you?"
)