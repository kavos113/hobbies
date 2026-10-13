#ifndef REFACTOR_VULKANCONTEXT_H
#define REFACTOR_VULKANCONTEXT_H

#include <memory>

#include <vulkan/vulkan.h>
#include <vk_mem_alloc.h>

#include "VulkanDebug.h"

class VulkanContext
{
public:
    VulkanContext();
    ~VulkanContext();

    int findQueueFamilies() const;

    VkCommandBuffer beginSingleTimeCommands() const;
    void endSingleTimeCommands(VkCommandBuffer commandBuffer) const;

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

    VkDescriptorPool descriptorPool() const
    {
        return m_descriptorPool;
    }

    VkCommandPool commandPool() const
    {
        return m_commandPool;
    }

    VmaAllocator allocator() const
    {
        return m_allocator;
    }

private:
    void createInstance();
    void pickPhysicalDevice();
    void createLogicalDevice();
    void createCommandPool();
    void createDescriptorPool();
    void createVmaAllocator();

    std::unique_ptr<VulkanDebug> m_debug;

    VkInstance m_instance = VK_NULL_HANDLE;
    VkPhysicalDevice m_physicalDevice = VK_NULL_HANDLE;
    VkDevice m_device = VK_NULL_HANDLE;
    VkQueue m_graphicsQueue = VK_NULL_HANDLE;
    VmaAllocator m_allocator = VK_NULL_HANDLE;

    VkCommandPool m_commandPool = VK_NULL_HANDLE;
    VkDescriptorPool m_descriptorPool = VK_NULL_HANDLE;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;

#ifdef NDEBUG
    const bool m_enableValidationLayers = false;
#else
    const bool m_enableValidationLayers = true;
#endif
};


#endif //REFACTOR_VULKANCONTEXT_H
