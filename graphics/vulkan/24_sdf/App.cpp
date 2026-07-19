#include "App.h"

App::App(size_t entryIndex)
    : m_window(nullptr)
{
    createWindow();

    m_context = std::make_unique<VulkanContext>();
    m_engine = std::make_unique<VulkanEngine>(m_window, m_context.get(), entryIndex);
}

App::~App()
{
    m_engine.reset();
    m_context.reset();

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
    glfwSetWindowUserPointer(m_window, this);
    glfwSetFramebufferSizeCallback(m_window, framebufferResizeCallback);
}

void App::framebufferResizeCallback(GLFWwindow* window, int width, int height)
{
    auto app = static_cast<App*>(glfwGetWindowUserPointer(window));
    app->m_engine->isResized = true;
}
