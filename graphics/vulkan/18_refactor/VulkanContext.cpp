#include "VulkanContext.h"

#include <cstdint>

#include <stdexcept>
#include <vector>
#include <ranges>
#include <string>
#include <iostream>
#include <algorithm>
#include <map>

#include <GLFW/glfw3.h>

namespace
{
std::vector<std::string> requiredDeviceExtensions = {
    VK_KHR_SWAPCHAIN_EXTENSION_NAME
};

int _findQueueFamilies(VkPhysicalDevice device)
{
    uint32_t queueFamilyCount = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, nullptr);
    std::vector<VkQueueFamilyProperties> queueFamilies(queueFamilyCount);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, queueFamilies.data());

    auto graphicsQueueFamilyIt = std::ranges::find_if(queueFamilies, [](const VkQueueFamilyProperties& queueFamily)
    {
        return queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT;
    });

    if (graphicsQueueFamilyIt == queueFamilies.end())
    {
        return -1;
    }
    else
    {
        return static_cast<int>(std::distance(queueFamilies.begin(), graphicsQueueFamilyIt));
    }
}

int rateDeviceSuitability(VkPhysicalDevice device)
{
    VkPhysicalDeviceProperties  deviceProperties;
    VkPhysicalDeviceFeatures deviceFeatures;
    vkGetPhysicalDeviceProperties(device, &deviceProperties);
    vkGetPhysicalDeviceFeatures(device, &deviceFeatures);

    int score = 0;

    if (!deviceFeatures.geometryShader)
    {
        return 0;
    }

    if (_findQueueFamilies(device) == -1)
    {
        return 0;
    }

    if (deviceProperties.apiVersion < VK_API_VERSION_1_3)
    {
        score -= 10000;
    }

    if (deviceProperties.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
    {
        score += 1000;
    }

    if (deviceProperties.deviceType == VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
    {
        score += 100;
    }

    return score;
}
}

VulkanContext::VulkanContext()
{
    createInstance();

    if (m_enableValidationLayers)
    {
        m_debug = std::make_unique<VulkanDebug>(m_instance);
    }

    pickPhysicalDevice();
    createLogicalDevice();
    createDescriptorPool();
    createCommandPool();
}

VulkanContext::~VulkanContext()
{
    vkDestroyDescriptorPool(m_device, m_descriptorPool, nullptr);
    vkDestroyCommandPool(m_device, m_commandPool, nullptr);

    vkDestroyDevice(m_device, nullptr);

    m_debug->cleanup(m_instance);
    m_debug.reset();

    vkDestroyInstance(m_instance, nullptr);
}

int VulkanContext::findQueueFamilies() const
{
    return _findQueueFamilies(m_physicalDevice);
}

void VulkanContext::createInstance()
{
    VkApplicationInfo appInfo = {
        .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = "Hello Vulkan",
        .applicationVersion = VK_MAKE_VERSION(1, 0, 0),
        .pEngineName = "No Engine",
        .engineVersion = VK_MAKE_VERSION(1, 0, 0),
        .apiVersion = VK_API_VERSION_1_3
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

void VulkanContext::pickPhysicalDevice()
{
    uint32_t deviceCount = 0;
    vkEnumeratePhysicalDevices(m_instance, &deviceCount, nullptr);
    if (deviceCount == 0)
    {
        throw std::runtime_error("Failed to find any Vulkan-compatible GPU");
    }

    std::vector<VkPhysicalDevice> devices(deviceCount);
    vkEnumeratePhysicalDevices(m_instance, &deviceCount, devices.data());

    std::multimap<int, VkPhysicalDevice> candidates;
    for (const auto& device : devices)
    {
        int score = rateDeviceSuitability(device);
        candidates.insert({score, device});
    }

    if (candidates.rbegin()->first > 0)
    {
        m_physicalDevice = candidates.rbegin()->second;
    }
    else
    {
        throw std::runtime_error("Failed to find a suitable GPU");
    }
}

void VulkanContext::createLogicalDevice()
{
    int graphicsQueueFamilyIndex = _findQueueFamilies(m_physicalDevice);
    if (graphicsQueueFamilyIndex == -1)
    {
        throw std::runtime_error("Failed to find a queue family that supports graphics commands");
    }

    float queuePriority = 1.0f;
    VkDeviceQueueCreateInfo queueCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        .queueFamilyIndex = static_cast<uint32_t>(graphicsQueueFamilyIndex),
        .queueCount = 1,
        .pQueuePriorities = &queuePriority
    };

    VkPhysicalDeviceExtendedDynamicStateFeaturesEXT extendedDynamicStateFeatures = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT,
        .extendedDynamicState = VK_TRUE
    };
    VkPhysicalDeviceVulkan13Features deviceFeatures = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
        .pNext = &extendedDynamicStateFeatures,
        .synchronization2 = VK_TRUE,
        .dynamicRendering = VK_TRUE
    };
    VkPhysicalDeviceVulkan11Features device11Features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
        .pNext = &deviceFeatures,
        .shaderDrawParameters = VK_TRUE
    };
    VkPhysicalDeviceFeatures2 deviceFeatures2 = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
        .pNext = &device11Features
    };

    std::vector<const char*> enabledExtensionNamePtrs = requiredDeviceExtensions
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();

    VkDeviceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &deviceFeatures2,
        .queueCreateInfoCount = 1,
        .pQueueCreateInfos = &queueCreateInfo,
        .enabledExtensionCount = static_cast<uint32_t>(enabledExtensionNamePtrs.size()),
        .ppEnabledExtensionNames = enabledExtensionNamePtrs.data()
    };
    VkResult r = vkCreateDevice(m_physicalDevice, &createInfo, nullptr, &m_device);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create logical device");
    }

    vkGetDeviceQueue(m_device, graphicsQueueFamilyIndex, 0, &m_graphicsQueue);
}

void VulkanContext::createCommandPool()
{
    int graphicsQueueFamilyIndex = _findQueueFamilies(m_physicalDevice);
    if (graphicsQueueFamilyIndex == -1)
    {
        throw std::runtime_error("Failed to find a queue family that supports graphics commands");
    }

    VkCommandPoolCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = static_cast<uint32_t>(graphicsQueueFamilyIndex)
    };
    VkResult r = vkCreateCommandPool(m_device, &createInfo, nullptr, &m_commandPool);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create command pool");
    }
}

void VulkanContext::createDescriptorPool()
{
    VkDescriptorPoolSize poolSize = {
        .type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        .descriptorCount = static_cast<uint32_t>(MAX_FRAMES_IN_FLIGHT)
    };
    VkDescriptorPoolCreateInfo poolInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        .maxSets = static_cast<uint32_t>(MAX_FRAMES_IN_FLIGHT),
        .poolSizeCount = 1,
        .pPoolSizes = &poolSize
    };
    VkResult r = vkCreateDescriptorPool(m_device, &poolInfo, nullptr, &m_descriptorPool);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create descriptor pool");
    }
}
