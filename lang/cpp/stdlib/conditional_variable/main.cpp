#include <print>
#include <thread>
#include <mutex>
#include <chrono>
#include <condition_variable>

int main()
{
    std::mutex mtx;
    std::condition_variable_any cv;

    std::jthread th([&mtx, &cv](const std::stop_token& s)
    {
        for (int i = 0; i < 10; i++)
        {
            std::println("this is timer {}", i);

            std::unique_lock lock(mtx);
            cv.wait_for(lock, s, std::chrono::seconds(1), []
            {
                return false;
            });

            if (s.stop_requested())
            {
                std::println("return early");
                return;
            }
        }
    });

    std::this_thread::sleep_for(std::chrono::milliseconds(3500));
    std::println("sending interrupt");
    th.request_stop();
    return 0;
}