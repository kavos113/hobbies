#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>

#include "schema.grpc.pb.h"
#include "schema.pb.h"

#include <format>
#include <iostream>
#include <memory>
#include <string>

using hellostreaming::HelloRequest;
using hellostreaming::HelloResponse;
using hellostreaming::HelloService;

class HelloServiceImpl final : public HelloService::Service
{
public:
    grpc::Status SayHello(
        grpc::ServerContext* context,
        const HelloRequest* request,
        grpc::ServerWriter<HelloResponse>* writer
    ) override
    {
        std::string response_text = "これは，gRPCのストリーミングレスポンスのデモです。サーバーから一文字ずつレスポンスが送られていきます。";

        for (size_t i = 0; i < response_text.length(); )
        {
            if (context->IsCancelled())
            {
                return grpc::Status::CANCELLED;
            }

            size_t len = GetCharLength(response_text[i]);
            if (i + len > response_text.length())
            {
                len = response_text.length() - i;
            }

            HelloResponse response;
            std::string res_str = response_text.substr(i, len);
            response.set_response(res_str);

            writer->Write(response);

            std::this_thread::sleep_for(std::chrono::milliseconds(100));

            i += len;
        }

        return grpc::Status::OK;
    }

private:
    size_t GetCharLength(char c)
    {
        if ((c & 0x80) == 0) return 1;
        if ((c & 0xE0) == 0xC0) return 2;
        if ((c & 0xF0) == 0xE0) return 3;
        if ((c & 0xF8) == 0xF0) return 4;
        return 1;
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

    return 0;
}