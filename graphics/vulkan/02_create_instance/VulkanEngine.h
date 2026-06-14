#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <vector>
#include <string>

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

    const std::vector<std::string> m_validationLayers = {
        "VK_LAYER_KHRONOS_validation"
    };
#ifdef NDEBUG
    const bool m_enableValidationLayers = false;
#else
    const bool m_enableValidationLayers = true;
#endif
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
