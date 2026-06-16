#ifndef HELLO_GLFW_APP_H
#define HELLO_GLFW_APP_H

#include <memory>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include "VulkanEngine.h"

class App
{
public:
    App();
    ~App();

    void run();

private:
    void createWindow();

    GLFWwindow* m_window;
    std::unique_ptr<VulkanEngine> m_engine;
};


#endif //HELLO_GLFW_APP_H
