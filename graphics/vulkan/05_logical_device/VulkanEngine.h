#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <memory>

#include <vulkan/vulkan.h>

#include "VulkanDebug.h"

class VulkanEngine
{
public:
    VulkanEngine();
    ~VulkanEngine();

    void render();

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


#endif //CREATE_INSTANCE_VULKANENGINE_H
