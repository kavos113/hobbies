cmake_minimum_required(VERSION 3.29)
project(winsock-window CXX)

add_executable(winsock-window WIN32 udp.cpp
        BaseWindow.h
        MainWindow.cpp
        MainWindow.h)
target_link_libraries(winsock-window PUBLIC WS2_32.lib)
target_link_libraries(winsock-window PUBLIC MsWSock.lib)
target_link_libraries(winsock-window PUBLIC AdvAPI32.lib)
target_link_libraries(winsock-window PUBLIC d2d1.lib)