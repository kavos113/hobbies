#include <grpcpp/grpcpp.h>

#include "schema.pb.h"
#include "schema.grpc.pb.h"

#include <memory>

using hellostreaming::HelloRequest;
using hellostreaming::HelloResponse;
using hellostreaming::HelloService;

class HelloClient
{
public:
    HelloClient(const std::shared_ptr<grpc::Channel>& channel)
        : m_stub(HelloService::NewStub(channel))
    {

    }

    void SayHello(const std::string& request_str)
    {
        HelloRequest request;
        request.set_request(request_str);

        grpc::ClientContext ctx;
        HelloResponse res;

        std::unique_ptr reader(m_stub->SayHello(&ctx, request));
        while (reader->Read(&res))
        {
            std::cout << res.response() << std::flush;
        }

        auto status = reader->Finish();

        if (status.ok())
        {
            std::cout << "\nFinish." << std::endl;
        }
        else
        {
            std::cout << "\nrpc failed: " << status.error_code() << ": " << status.error_message() << std::endl;
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

    client.SayHello("input");

    return 0;
}