cmake_minimum_required(VERSION 3.28)
project(winhttp)

set(CMAKE_CXX_STANDARD 20)

function(add_winhttp_executable name src)
    add_executable(${name} ${src})
    target_link_libraries(${name} PRIVATE winhttp)
endfunction()

add_winhttp_executable(winhttp_sendrequest SendRequest.cpp)