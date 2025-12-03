cmake_minimum_required(VERSION 3.28)
project(x-hello3)

add_executable(x-hello3 main.c)

if (MSVC)
    target_include_directories(x-hello3 PRIVATE $ENV{X_INCLUDE_DIR})
else ()
    target_include_directories(x-hello3 PRIVATE /usr/include)
    target_link_libraries(x-hello3 PRIVATE /usr/lib/x86_64-linux-gnu/libX11.so)
endif ()
