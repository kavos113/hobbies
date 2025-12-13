cmake_minimum_required(VERSION 3.28)
project(grpc-helloworld)

set(CMAKE_CXX_STANDARD 20)

find_package(gRPC CONFIG REQUIRED)

foreach (_target
    server
    client
)
    set(target_name grpc-helloworld-${_target})
    add_executable(${target_name} ${_target}.cpp)

    target_link_libraries(${target_name} PRIVATE
        gRPC::grpc++
    )
endforeach ()
