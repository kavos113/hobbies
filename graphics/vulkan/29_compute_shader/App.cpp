#include "App.h"

App::App()
{
    m_context = std::make_unique<VulkanContext>();
    m_engine = std::make_unique<VulkanEngine>(m_context.get());
}

App::~App() = default;

void App::run()
{
    m_engine->run();
}

