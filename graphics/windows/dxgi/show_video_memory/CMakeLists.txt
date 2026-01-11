cmake_minimum_required(VERSION 3.28)
project(dxgi_show_video_memory)

set(CMAKE_CXX_STANDARD 20)

add_executable(dxgi_show_video_memory main.cpp)
target_link_libraries(dxgi_show_video_memory PRIVATE dxgi)