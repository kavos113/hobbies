cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 01_hello)
project(xinput_${EXAMPLE_NAME})

add_executable(xinput_${EXAMPLE_NAME} main.cpp)
target_link_libraries(xinput_${EXAMPLE_NAME} Xinput)
set_target_properties(xinput_${EXAMPLE_NAME} PROPERTIES FOLDER xinput)
