#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <memory>

#include <vulkan/vulkan.h>

#define NOMINMAX
#define VK_USE_PLATFORM_WIN32_KHR
#define GLFW_INCLUDE_VULKAN
#include <glfw/glfw3.h>
#define GLFW_EXPOSE_NATIVE_WIN32
#include <glfw/glfw3native.h>

#include "VulkanDebug.h"

class VulkanEngine
{
public:
    VulkanEngine(GLFWwindow* window);
    ~VulkanEngine();

    void render();

private:
    void createInstance();
    void pickPhysicalDevice();
    void createLogicalDevice();
    void createSurface(GLFWwindow* window);
    void createSwapchain(GLFWwindow* window);

    VkSurfaceFormatKHR chooseSwapSurfaceFormat() const;
    VkPresentModeKHR chooseSwapPresentMode();
    VkExtent2D chooseSwapExtent(GLFWwindow* window);

    std::unique_ptr<VulkanDebug> m_debug;

    VkInstance m_instance = VK_NULL_HANDLE;
    VkPhysicalDevice m_physicalDevice = VK_NULL_HANDLE;
    VkDevice m_device = VK_NULL_HANDLE;
    VkQueue m_graphicsQueue = VK_NULL_HANDLE;

    VkSurfaceKHR m_surface = VK_NULL_HANDLE;
    VkSwapchainKHR m_swapchain = VK_NULL_HANDLE;
    std::vector<VkImage> m_swapchainImages;
    VkSurfaceFormatKHR m_swapchainImageFormat;
    VkExtent2D m_swapchainExtent;

#ifdef NDEBUG
    const bool m_enableValidationLayers = false;
#else
    const bool m_enableValidationLayers = true;
#endif
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
