cmake_minimum_required(VERSION 3.31)
project(vulkan_playground)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(THIRD_PARTY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty)
set(RESOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/resources)

#add_subdirectory(old)