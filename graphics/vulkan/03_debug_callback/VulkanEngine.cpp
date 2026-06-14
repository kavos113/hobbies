#include "VulkanEngine.h"

#include <cstdint>

#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <ranges>

#include <glfw/glfw3.h>

VulkanEngine::VulkanEngine()
{
    createInstance();

    if (m_enableValidationLayers)
    {
        m_debug = std::make_unique<VulkanDebug>(m_instance);
    }
}

VulkanEngine::~VulkanEngine()
{
    m_debug->cleanup(m_instance);
    m_debug.reset();

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

    std::vector<std::string> enabledLayerNames;
    std::vector<std::string> enabledExtensionNames(glfwExtensions, glfwExtensions + glfwExtensionCount);

    if (m_enableValidationLayers)
    {
        VulkanDebug::addDebugSettings(enabledExtensionNames, enabledLayerNames);
    }

    std::vector<const char*> enabledLayerNamePtrs = enabledLayerNames
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();
    std::vector<const char*> enabledExtensionNamePtrs = enabledExtensionNames
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();

    VkInstanceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &appInfo,
        .enabledLayerCount = static_cast<uint32_t>(enabledLayerNamePtrs.size()),
        .ppEnabledLayerNames = enabledLayerNamePtrs.data(),
        .enabledExtensionCount = static_cast<uint32_t>(enabledExtensionNamePtrs.size()),
        .ppEnabledExtensionNames = enabledExtensionNamePtrs.data()
    };

    VkResult r = vkCreateInstance(&createInfo, nullptr, &m_instance);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create Vulkan instance");
    }
}
