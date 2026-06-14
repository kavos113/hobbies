cmake_minimum_required(VERSION 3.28)
project(01_hello_glfw)

set(CMAKE_CXX_STANDARD 20)

add_executable(01_hello_glfw
        main.cpp
        App.h
        App.cpp
)
target_link_libraries(01_hello_glfw PRIVATE
        glfw
        Vulkan::Vulkan
)
