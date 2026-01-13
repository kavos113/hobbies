cmake_minimum_required(VERSION 3.28)
project(winui-cmake-hello)

set(CMAKE_CXX_STANDARD 20)

add_executable(winui-cmake-hello WIN32 main.cpp)
target_include_directories(winui-cmake-hello PRIVATE ${THIRD_PARTY_DIR}/include_gen)
target_link_libraries(winui-cmake-hello PRIVATE windowsapp)