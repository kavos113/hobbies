#include "VulkanEngine.h"

#include <stdexcept>

VulkanEngine::VulkanEngine()
{
    createInstance();
}

VulkanEngine::~VulkanEngine()
{
    vkDestroyInstance(m_instance, nullptr);
}

void VulkanEngine::render()
{
}

void VulkanEngine::createInstance()
{
    VkApplicationInfo appInfo = {
        .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = "Hello Vulkan",
        .applicationVersion = VK_MAKE_VERSION(1, 0, 0),
        .pEngineName = "No Engine",
        .engineVersion = VK_MAKE_VERSION(1, 0, 0),
        .apiVersion = VK_API_VERSION_1_0
    };

    VkInstanceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &appInfo
    };

    VkResult r = vkCreateInstance(&createInfo, nullptr, &m_instance);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create Vulkan instance");
    }
}
