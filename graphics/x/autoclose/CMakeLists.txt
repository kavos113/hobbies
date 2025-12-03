cmake_minimum_required(VERSION 3.28)
project(x-autoclose)

add_executable(x-autoclose main.c)

if (MSVC)
    target_include_directories(x-autoclose PRIVATE $ENV{X_INCLUDE_DIR})
else ()
    target_include_directories(x-autoclose PRIVATE /usr/include)
    target_link_libraries(x-autoclose PRIVATE /usr/lib/x86_64-linux-gnu/libX11.so)
endif ()
