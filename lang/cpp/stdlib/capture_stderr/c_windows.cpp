#include <iostream>
#include <cstdio>
#include <io.h>
#include <fcntl.h>
#include <thread>

class capturer
{
public:
    void start()
    {
        if (_pipe(m_pipeFds, 4096, O_TEXT) != 0)
        {
            std::cout << "error create pipe" << std::endl;
            return;
        }

        original_stderr = _dup(_fileno(stderr));

        // write descriptor: stderr -> m_pipeFds
        if (_dup2(m_pipeFds[1], _fileno(stderr)) != 0)
        {
            std::cout << "error in dup2" << std::endl;
            return;
        }

        setvbuf(stderr, nullptr, _IONBF, 0);

        m_capturing = true;

        worker = std::thread(&capturer::readData, this);
    }

    void stop()
    {
        if (!m_capturing)
        {
            return;
        }

        m_capturing = false;

        if (original_stderr != -1)
        {
            _dup2(original_stderr, _fileno(stderr));
            _close(original_stderr);
            original_stderr = -1;
        }

        if (m_pipeFds[0] != -1)
        {
            _close(m_pipeFds[0]);
            m_pipeFds[0] = -1;
        }

        if (m_pipeFds[1] != -1)
        {
            _close(m_pipeFds[1]);
            m_pipeFds[1] = -1;
        }

        if (worker.joinable())
        {
            worker.join();
        }
    }
private:
    void readData()
    {
        char buf[1024];

        while (m_capturing)
        {
            int bytes_read = _read(m_pipeFds[0], buf, sizeof(buf) - 1);
            std::cout << bytes_read << std::endl;

            if (bytes_read > 0)
            {
                buf[bytes_read] = 0;
                std::string str(buf);

                std::cout << "message from cerr: " << str << std::endl;
            }
            else
            {
                break;
            }
        }
    }

    bool m_capturing = false;

    int m_pipeFds[2] = {-1, -1};
    int original_stderr = -1;

    std::thread worker;
};

void stderr_output_function()
{
    std::cerr << "output to cerr" << std::endl;
    std::cout << "output to cout" << std::endl;
}

void c_stderr_output_function()
{
    fprintf(stderr, "output to stderr\n");
}

int main()
{
    capturer c;
    c.start();

    stderr_output_function();
    c_stderr_output_function();

    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    c.stop();

    return 0;
}