#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>

#include "schema.grpc.pb.h"
#include "schema.pb.h"

#include <format>
#include <iostream>
#include <memory>
#include <string>

using helloworld::HelloService;
using helloworld::HelloRequest;
using helloworld::HelloResponse;

class HelloServiceImpl final : public HelloService::Service
{
    grpc::Status SayHello(
        grpc::ServerContext* context,
        const HelloRequest* request,
        HelloResponse* response
    ) override
    {
        std::stringstream ss;
        ss << "hello, " << request->name();

        response->set_message(ss.str());
        return grpc::Status::OK;
    }
};

int main()
{
    constexpr uint16_t port = 50051;

    std::string server_address = std::format("0.0.0.0:{}", port);

    HelloServiceImpl service;

    grpc::EnableDefaultHealthCheckService(true);
    grpc::reflection::InitProtoReflectionServerBuilderPlugin();

    grpc::ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);

    std::unique_ptr server(builder.BuildAndStart());
    std::cout << "server is listening on" << server_address << std::endl;

    server->Wait();
}