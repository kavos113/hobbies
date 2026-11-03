#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <memory>

#include <vulkan/vulkan.h>

#include "VulkanContext.h"

class VulkanEngine
{
public:
    VulkanEngine(VulkanContext *context);
    ~VulkanEngine();

    void run();

private:

    void createSyncObjects();
    void createPipeline();
    void createCommandBuffer();

    void recordCommandBuffer();

    VkShaderModule createShaderModule(const std::string& filepath) const;

    VulkanContext *m_context;

    VkPipelineLayout m_pipelineLayout = VK_NULL_HANDLE;
    VkPipeline m_pipeline = VK_NULL_HANDLE;

    VkCommandBuffer m_commandBuffer;
    VkSemaphore m_computeFinishedSemaphore;
    VkFence m_fence;
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
