#ifndef DEBUG_CALLBACK_VULKANDEBUG_H
#define DEBUG_CALLBACK_VULKANDEBUG_H

#include <string>
#include <vector>

#include <vulkan/vulkan.h>

class VulkanDebug
{
public:
    VulkanDebug(VkInstance instance);
    ~VulkanDebug();

    void cleanup(VkInstance instance);

    static void addDebugSettings(
        std::vector<std::string>& extensions,
        std::vector<std::string>& layers
    );

private:
    VkDebugUtilsMessengerEXT m_debugMessenger = VK_NULL_HANDLE;

    static const std::vector<std::string> VALIDATION_LAYERS;
};


#endif //DEBUG_CALLBACK_VULKANDEBUG_H
