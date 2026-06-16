#ifndef REFACTOR_VULKANCONTEXT_H
#define REFACTOR_VULKANCONTEXT_H

#include <memory>

#include <vulkan/vulkan.h>

#include "VulkanDebug.h"

class VulkanContext
{
public:
    VulkanContext();
    ~VulkanContext();

    int findQueueFamilies() const;

    VkInstance instance() const
    {
        return m_instance;
    }

    VkPhysicalDevice physicalDevice() const
    {
        return m_physicalDevice;
    }

    VkDevice device() const
    {
        return m_device;
    }

    VkQueue graphicsQueue() const
    {
        return m_graphicsQueue;
    }

private:
    void createInstance();
    void pickPhysicalDevice();
    void createLogicalDevice();

    std::unique_ptr<VulkanDebug> m_debug;

    VkInstance m_instance = VK_NULL_HANDLE;
    VkPhysicalDevice m_physicalDevice = VK_NULL_HANDLE;
    VkDevice m_device = VK_NULL_HANDLE;
    VkQueue m_graphicsQueue = VK_NULL_HANDLE;


#ifdef NDEBUG
    const bool m_enableValidationLayers = false;
#else
    const bool m_enableValidationLayers = true;
#endif
};


#endif //REFACTOR_VULKANCONTEXT_H
