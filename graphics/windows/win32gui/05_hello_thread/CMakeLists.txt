cmake_minimum_required(VERSION 3.29)
project(win32_05_hello_thread LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

add_executable(win32_05_hello_thread WIN32 main.cpp)