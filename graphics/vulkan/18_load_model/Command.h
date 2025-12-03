#ifndef REFACTOR2_COMMAND_H
#define REFACTOR2_COMMAND_H

#include <optional>
#include <vector>
#include <vulkan/vulkan.h>

#include "QueueFamilyIndices.h"
#include "Swapchain.h"

class Command
{
public:
    static std::vector<VkDeviceQueueCreateInfo> queueCreateInfo(VkPhysicalDevice device, VkSurfaceKHR surface);

    void create(VkDevice device, VkPhysicalDevice physicalDevice, VkSurfaceKHR surface);
    void claenup();

    void beginRender(uint32_t currentFrame);
    void endRender(uint32_t currentFrame, const Swapchain &swapchain);

    VkResult present(VkPresentInfoKHR &presentInfo, uint32_t currentFrame);

    void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size);
    void copyBufferToImage(VkBuffer buffer, VkImage image, uint32_t width, uint32_t height);
    void transitionImageLayout(
        VkImage image,
        VkFormat format,
        VkImageLayout oldLayout,
        VkImageLayout newLayout
    );

    VkCommandBuffer commandBuffer(uint32_t currentFrame) const
    {
        return commandBuffers[currentFrame];
    }

private:
    void setQueue(QueueFamilyIndices indices);
    void createCommandPool(QueueFamilyIndices queueFamilyIndices);
    void createCommandBuffer();
    void createSyncObjects();

    VkCommandBuffer beginSingleTimeCommands();
    void endSingleTimeCommands(VkCommandBuffer commandBuffer);

    bool hasStencilComponent(VkFormat format);

    VkDevice m_device = VK_NULL_HANDLE;
    VkCommandPool commandPool = VK_NULL_HANDLE;
    std::vector<VkCommandBuffer> commandBuffers;
    std::vector<VkSemaphore> renderFinishedSemaphores;
    std::vector<VkFence> inFlightFences;
    VkQueue graphicsQueue = VK_NULL_HANDLE;
    VkQueue presentQueue = VK_NULL_HANDLE;
};



#endif //REFACTOR2_COMMAND_H
