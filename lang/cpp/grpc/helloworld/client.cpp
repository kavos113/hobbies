#include <grpcpp/grpcpp.h>

#include "schema.pb.h"
#include "schema.grpc.pb.h"

#include <memory>

using helloworld::HelloService;
using helloworld::HelloRequest;
using helloworld::HelloResponse;

class HelloClient
{
public:
    HelloClient(const std::shared_ptr<grpc::Channel>& channel)
        : m_stub(HelloService::NewStub(channel))
    {

    }

    std::string SayHello(const std::string& user)
    {
        HelloRequest request;
        request.set_name(user);

        HelloResponse response;
        grpc::ClientContext ctx;

        auto status = m_stub->SayHello(&ctx, request, &response);

        if (status.ok())
        {
            return response.message();
        }
        else
        {
            std::cout << status.error_code() << ": " << status.error_message() << std::endl;
            return "rpc failed";
        }
    }

private:
    std::unique_ptr<HelloService::Stub> m_stub;
};

int main()
{
    const std::string target = "localhost:50051";

    HelloClient client(
        grpc::CreateChannel(target, grpc::InsecureChannelCredentials())
    );
    std::string user("world");
    std::string res = client.SayHello(user);
    std::cout << "result: " << res << std::endl;

    return 0;
}