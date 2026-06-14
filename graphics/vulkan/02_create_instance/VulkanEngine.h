#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <vulkan/vulkan.h>

class VulkanEngine
{
public:
    VulkanEngine();
    ~VulkanEngine();

    void render();

private:
    void createInstance();

    VkInstance m_instance = VK_NULL_HANDLE;
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
