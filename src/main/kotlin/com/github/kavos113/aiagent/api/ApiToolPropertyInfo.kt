package com.github.kavos113.aiagent.api

import com.openai.core.JsonValue

/*
json definition:
"parameters": {
     "type": "object",
     "properties": {
         "list": {
             "type": "array",
             "items": {"type": "string"},
             "description": "List",
         },
         "date": {
             "type": "string",
             "description": "Date",
         },
     },
     "required": ["list"],
 },

this will be converted to:
ListOf(
    ApiToolPropertyInfo(
        name = "list",
        type = "array",
        description = "List",
        required = true,
        other = mapOf("items" to JsonObject(mapOf("type" to JsonString("string"))))
    ),
    ApiToolPropertyInfo(
        name = "date",
        type = "string",
        description = "Date",
        required = false,
    )
)

 */
data class ApiToolPropertyInfo(
    val name: String,
    val type: String,
    val description: String?,
    val required: Boolean,
    val other: Map<String, JsonValue>?
)