#include "VulkanEngine.h"

#include <cstdint>

#include <algorithm>
#include <stdexcept>
#include <vector>
#include <iostream>

#include <glfw/glfw3.h>

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

    uint32_t glfwExtensionCount = 0;
    const char** glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);

    uint32_t extensionCount = 0;
    vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, nullptr);
    std::vector<VkExtensionProperties> availableExtensions(extensionCount);
    vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, availableExtensions.data());

    for (uint32_t i = 0; i < glfwExtensionCount; i++)
    {
        bool notFound = std::ranges::none_of(availableExtensions, [glfwExtension = glfwExtensions[i]](const VkExtensionProperties& extension)
        {
            return strcmp(glfwExtension, extension.extensionName) == 0;
        });
        if (notFound)
        {
            throw std::runtime_error("Required Vulkan extension not found: " + std::string(glfwExtensions[i]));
        }
    }

    uint32_t layerCount;
    vkEnumerateInstanceLayerProperties(&layerCount, nullptr);
    std::vector<VkLayerProperties> availableLayers(layerCount);
    vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.data());

    for (const auto& layer : availableLayers)
    {
        std::cout << "Available layer: " << layer.layerName << std::endl;
    }

    VkInstanceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &appInfo,
        .enabledLayerCount = 0,
        .ppEnabledLayerNames = nullptr,
        .enabledExtensionCount = glfwExtensionCount,
        .ppEnabledExtensionNames = glfwExtensions,
    };

    VkResult r = vkCreateInstance(&createInfo, nullptr, &m_instance);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create Vulkan instance");
    }
}
