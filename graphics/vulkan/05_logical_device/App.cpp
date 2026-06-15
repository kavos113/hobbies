#include "App.h"

App::App()
    : m_window(nullptr)
{
    createWindow();

    m_engine = std::make_unique<VulkanEngine>();
}

App::~App()
{
    glfwDestroyWindow(m_window);
    glfwTerminate();
}

void App::run()
{
    while (!glfwWindowShouldClose(m_window))
    {
        glfwPollEvents();
        m_engine->render();
    }
}

void App::createWindow()
{
    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    m_window = glfwCreateWindow(800, 600, "Hello GLFW", nullptr, nullptr);
}
