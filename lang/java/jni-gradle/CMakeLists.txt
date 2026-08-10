cmake_minimum_required(VERSION 3.28)
project(javamodule CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Java REQUIRED)
find_package(JNI REQUIRED)

add_library(javamodule SHARED
        src/main/cpp/jni_Main.cpp
        src/main/cpp/jni_Main.h
)

target_include_directories(javamodule PRIVATE ${JNI_INCLUDE_DIRS})
target_compile_options(javamodule PRIVATE "/utf-8")

set(LIB_OUTPUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/src/main/resources/javamodule.dll")

add_custom_command(TARGET javamodule POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "$<TARGET_FILE:javamodule>"
            "${LIB_OUTPUT_FILE}"
        COMMENT "copy dll file"
)
