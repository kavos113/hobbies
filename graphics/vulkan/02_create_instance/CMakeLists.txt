cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 02_create_instance)

project(${EXAMPLE_NAME} LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

add_executable(${EXAMPLE_NAME}
        main.cpp
        App.h
        App.cpp
        VulkanEngine.cpp
        VulkanEngine.h
)
target_link_libraries(${EXAMPLE_NAME} PRIVATE
        glfw
        Vulkan::Vulkan
)
