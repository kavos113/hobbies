cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 20_dynamic_uniform_buffer)

project(${EXAMPLE_NAME} LANGUAGES CXX)

add_executable(${EXAMPLE_NAME}
        main.cpp
        App.h
        App.cpp
        VulkanEngine.cpp
        VulkanEngine.h
        VulkanDebug.cpp
        VulkanDebug.h
        VulkanContext.cpp
        VulkanContext.h
        VulkanHelper.h
        Object.cpp
        Object.h
)
target_link_libraries(${EXAMPLE_NAME} PRIVATE
        glfw
        glm::glm
        Vulkan::Vulkan
)
target_include_directories(${EXAMPLE_NAME} PRIVATE
        ${THIRD_PARTY_DIR}
)

find_program(SLANGC_EXECUTABLE NAMES slangc REQUIRED)

function(add_slang_shader TARGET)
    cmake_parse_arguments(
            "SHADER"
            ""
            ""
            "SOURCES"
            ${ARGN}
    )

    set(SHADER_OUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/shaders")

    add_custom_command(
            OUTPUT ${SHADER_OUT_DIR}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${SHADER_OUT_DIR}
            COMMENT "Creating shader output directory: ${SHADER_OUT_DIR}"
    )

    add_custom_command(
            OUTPUT ${SHADER_OUT_DIR}/shaders.spv
            COMMAND ${SLANGC_EXECUTABLE} ${SHADER_SOURCES} -target spirv -profile spirv_1_3 -emit-spirv-directly -fvk-use-entrypoint-name -entry vertMain -entry fragMain -o ${SHADER_OUT_DIR}/shaders.spv
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            DEPENDS ${SHADER_SOURCES} ${SHADER_OUT_DIR}
            COMMENT "Compiling shaders to SPIR-V: ${SHADER_SOURCES}"
            VERBATIM
    )

    add_custom_target(${TARGET} DEPENDS ${SHADER_OUT_DIR}/shaders.spv)
endfunction()

add_slang_shader(${EXAMPLE_NAME}_shaders
        SOURCES
        shader.slang
)
add_dependencies(${EXAMPLE_NAME} ${EXAMPLE_NAME}_shaders)

file(COPY ${RESOURCE_DIR} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(${EXAMPLE_NAME}_shaders PROPERTIES FOLDER shaders)