#ifndef HELLO_GLFW_APP_H
#define HELLO_GLFW_APP_H

#include <memory>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include "VulkanContext.h"
#include "VulkanEngine.h"

class App
{
public:
    App(size_t entryIndex);
    ~App();

    void run();

private:
    void createWindow();

    static void framebufferResizeCallback(GLFWwindow* window, int width, int height);

    GLFWwindow* m_window;
    std::unique_ptr<VulkanContext> m_context;
    std::unique_ptr<VulkanEngine> m_engine;
};


#endif //HELLO_GLFW_APP_H
