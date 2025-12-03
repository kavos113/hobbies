cmake_minimum_required(VERSION 3.28)
project(x-keyboard)

add_executable(x-keyboard main.c)

if (MSVC)
    target_include_directories(x-keyboard PRIVATE $ENV{X_INCLUDE_DIR})
else ()
    target_include_directories(x-keyboard PRIVATE /usr/include)
    target_link_libraries(x-keyboard PRIVATE /usr/lib/x86_64-linux-gnu/libX11.so)
endif ()
