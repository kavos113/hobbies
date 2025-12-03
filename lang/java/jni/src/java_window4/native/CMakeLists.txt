cmake_minimum_required(VERSION 3.29)
project(java_window4 CXX)

add_library(java_window4 SHARED java_component.cpp
        java_window.cpp
        util.cpp
        dx_factory.cpp
        jniutil.cpp
        java_text.cpp
        java_text.h)
target_include_directories(java_window4 PRIVATE $ENV{JAVA_HOME}/include $ENV{JAVA_HOME}/include/win32)