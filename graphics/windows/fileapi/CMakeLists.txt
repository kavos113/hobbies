cmake_minimum_required(VERSION 3.28)
project(fileapi)

set(CMAKE_CXX_STANDARD 20)

add_executable(fileapi_getdrivetype GetDriveType.cpp)
add_executable(fileapi_getdiskfreespace GetDiskFreeSpace.cpp)