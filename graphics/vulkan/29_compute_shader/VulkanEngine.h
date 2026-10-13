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
    VulkanContext *m_context;
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
