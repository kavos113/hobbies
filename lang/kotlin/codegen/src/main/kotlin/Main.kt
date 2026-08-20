package org.example

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import java.io.File

fun main() {
    val bytes = System.`in`.readBytes()
    File("out\\request.bin").writeBytes(bytes)

    val request = CodeGeneratorRequest.parseFrom(bytes)
    val responseBuilder = CodeGeneratorResponse.newBuilder()

    for (file in request.protoFileList) {
        for (msg in file.messageTypeList) {
            responseBuilder.addFile(
                CodeGeneratorResponse.File.newBuilder()
                    .setName("testgen/${msg.name}.kt")
                    .setContent(msg.fieldCount.toString())
                    .build()
            )
        }
    }

    System.err.println("called")

    responseBuilder.build().writeTo(System.out)
}
