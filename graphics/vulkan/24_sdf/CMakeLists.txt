cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 24_sdf)

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
        VulkanBuffer.h
        Object.cpp
        Object.h
        ShaderCompiler.cpp
        ShaderCompiler.h
)
target_link_libraries(${EXAMPLE_NAME} PRIVATE
        glfw
        glm::glm
        Vulkan::Vulkan
        slang::slang
)
target_include_directories(${EXAMPLE_NAME} PRIVATE
        ${THIRD_PARTY_DIR}
)

file(COPY vertex.slang DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY shaders DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
