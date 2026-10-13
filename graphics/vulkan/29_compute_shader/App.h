#ifndef HELLO_GLFW_APP_H
#define HELLO_GLFW_APP_H

#include <memory>

#include "VulkanContext.h"
#include "VulkanEngine.h"

class App
{
public:
    App();
    ~App();

    void run();

private:
    std::unique_ptr<VulkanEngine> m_engine;
    std::unique_ptr<VulkanContext> m_context;
};


#endif //HELLO_GLFW_APP_H
