cmake_minimum_required(VERSION 3.28)
project(win32_04_window_message)

set(CMAKE_CXX_STANDARD 20)

add_executable(win32_04_window_message
        main.cpp
        Application.cpp
        WindowMessage.cpp
)
