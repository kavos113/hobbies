#include <grpcpp/grpcpp.h>

#include "schema.grpc.pb.h"
#include "schema.pb.h"

#include <format>
#include <iostream>
#include <memory>
#include <string>

using helloasync::HelloResponse;
using helloasync::HelloRequest;
using helloasync::HelloService;

class ServerImpl final
{
public:
    ~ServerImpl()
    {
        m_server->Shutdown();
        m_cq->Shutdown();
    }

    void Run(uint16_t port)
    {
        std::string server_address = std::format("0.0.0.0:{}", port);

        grpc::ServerBuilder builder;
        builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
        builder.RegisterService(&m_service);

        m_cq = builder.AddCompletionQueue();
        m_server = builder.BuildAndStart();
        std::cout << "server is listening on" << server_address << std::endl;

        HandleRpcs();
    }

private:
    class CallData
    {
    public:
        CallData(
            HelloService::AsyncService *service,
            grpc::ServerCompletionQueue *cq
        )
            : m_service(service), m_cq(cq), m_responder(&m_ctx), m_status(CREATE)
        {
            Proceed();
        };

        void Proceed()
        {
            if (m_status == CREATE)
            {
                m_status = PROCESS;
                m_service->RequestSayHello(&m_ctx, &m_request, &m_responder, m_cq, m_cq, this);
            }
            else if (m_status == PROCESS)
            {
                new CallData(m_service, m_cq);

                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                std::string prefix("Hello ");
                m_response.set_message(prefix + m_request.name());

                m_status = FINISH;
                m_responder.Finish(m_response, grpc::Status::OK, this);
            }
            else
            {
                delete this;
            }
        }

    private:
        HelloService::AsyncService *m_service;
        grpc::ServerCompletionQueue *m_cq;

        grpc::ServerContext m_ctx;

        HelloRequest m_request;
        HelloResponse m_response;

        grpc::ServerAsyncResponseWriter<HelloResponse> m_responder;

        enum CallStatus { CREATE, PROCESS, FINISH };
        CallStatus m_status;
    };

    void HandleRpcs()
    {
        new CallData(&m_service, m_cq.get());
        void *tag;
        bool ok;

        while (true)
        {
            if (!m_cq->Next(&tag, &ok))
                break;

            if (!ok)
                break;

            static_cast<CallData *>(tag)->Proceed();
        }
    }

    std::unique_ptr<grpc::ServerCompletionQueue> m_cq;
    HelloService::AsyncService m_service;
    std::unique_ptr<grpc::Server> m_server;
};

int main()
{
    ServerImpl server;
    server.Run(50051);

    return 0;
}