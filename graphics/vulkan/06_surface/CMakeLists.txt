cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 06_surface)

project(${EXAMPLE_NAME} LANGUAGES CXX)

add_executable(${EXAMPLE_NAME}
        main.cpp
        App.h
        App.cpp
        VulkanEngine.cpp
        VulkanEngine.h
        VulkanDebug.cpp
        VulkanDebug.h
)
target_link_libraries(${EXAMPLE_NAME} PRIVATE
        glfw
        Vulkan::Vulkan
)
