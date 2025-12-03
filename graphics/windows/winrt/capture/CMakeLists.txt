cmake_minimum_required(VERSION 3.29)
project(capture CXX)

add_executable(capture WIN32 main.cpp
        Capture.cpp
        Capture.h)
