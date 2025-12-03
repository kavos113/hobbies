cmake_minimum_required(VERSION 3.28)
project(x-hello)

add_executable(x-hello main.c)

if (MSVC)
    target_include_directories(x-hello PRIVATE $ENV{X_INCLUDE_DIR})
else ()
    target_include_directories(x-hello PRIVATE /usr/include)
    target_link_libraries(x-hello PRIVATE /usr/lib/x86_64-linux-gnu/libX11.so)
endif ()
