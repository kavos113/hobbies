#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <vector>
#include <string>
#include <memory>

#include <vulkan/vulkan.h>

#define NOMINMAX
#define VK_USE_PLATFORM_WIN32_KHR
#define GLFW_INCLUDE_VULKAN
#include <glfw/glfw3.h>
#define GLFW_EXPOSE_NATIVE_WIN32
#include <glfw/glfw3native.h>

#include "VulkanContext.h"
#include "Object.h"
#include "ShaderCompiler.h"

class VulkanEngine
{
public:
    VulkanEngine(GLFWwindow* window, VulkanContext *context, const std::string& fsPath);
    ~VulkanEngine();

    void render();

    bool isResized = false;

private:
    void createSurface();
    void createSwapchain();
    void createImageViews();
    void createPipeline(const std::string& fsPath);
    void createCommandBuffer();
    void createSyncObjects();

    void recordCommandBuffer(uint32_t imageIndex) const;
    void recreateSwapchain();
    void cleanupSwapchain();

    VkSurfaceFormatKHR chooseSwapSurfaceFormat() const;
    VkPresentModeKHR chooseSwapPresentMode() const;
    VkExtent2D chooseSwapExtent(GLFWwindow* window) const;

    VkShaderModule createShaderModule(const std::string& filePath) const;

    VulkanContext *m_context;
    std::unique_ptr<Object> m_object;
    std::unique_ptr<ShaderCompiler> m_shaderCompiler;
    GLFWwindow* m_window = nullptr;

    VkSurfaceKHR m_surface = VK_NULL_HANDLE;
    VkSwapchainKHR m_swapchain = VK_NULL_HANDLE;
    std::vector<VkImage> m_swapchainImages;
    std::vector<VkImageView> m_swapchainImageViews;
    VkSurfaceFormatKHR m_swapchainImageFormat;
    VkExtent2D m_swapchainExtent;

    VkViewport m_viewport;
    VkRect2D m_scissor;

    VkPipelineLayout m_pipelineLayout = VK_NULL_HANDLE;
    VkPipeline m_graphicsPipeline = VK_NULL_HANDLE;

    std::vector<VkCommandBuffer> m_commandBuffers;
    std::vector<VkSemaphore> m_imageAvailableSemaphores;
    std::vector<VkSemaphore> m_renderCompleteSemaphores;
    std::vector<VkFence> m_drawFences;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
    const std::string VERTEX_SHADER = "vertex.slang";
    uint32_t m_currentFrame = 0;
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
