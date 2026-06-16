#ifndef CREATE_INSTANCE_VULKANENGINE_H
#define CREATE_INSTANCE_VULKANENGINE_H

#include <array>
#include <memory>
#include <vector>
#include <string>

#include <vulkan/vulkan.h>
#include <glm/glm.hpp>

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

    bool isResized = false;

private:
    struct Vertex
    {
        glm::vec2 pos;
        glm::vec3 color;

        static VkVertexInputBindingDescription getBindingDescription()
        {
            return {
                .binding = 0,
                .stride = sizeof(Vertex),
                .inputRate = VK_VERTEX_INPUT_RATE_VERTEX
            };
        }

        static std::array<VkVertexInputAttributeDescription, 2> getAttributeDescriptions()
        {
            return {
                VkVertexInputAttributeDescription{
                    .location = 0,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32_SFLOAT,
                    .offset = offsetof(Vertex, pos)
                },
                VkVertexInputAttributeDescription{
                    .location = 1,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32B32_SFLOAT,
                    .offset = offsetof(Vertex, color)
                }
            };
        }
    };

    const std::vector<Vertex> vertices = {
        {{-0.5f, -0.5f}, {1.0f, 0.0f, 0.0f}},
        {{0.5f, -0.5f}, {0.0f, 1.0f, 0.0f}},
        {{0.5f, 0.5f}, {0.0f, 0.0f, 1.0f}},
        {{-0.5f, 0.5f}, {1.0f, 1.0f, 1.0f}}
    };
    const std::vector<uint16_t> indices = {
        0, 1, 2,
        2, 3, 0
    };

private:
    void createInstance();
    void pickPhysicalDevice();
    void createLogicalDevice();
    void createSurface();
    void createSwapchain();
    void createImageViews();
    void createPipeline();
    void createCommandPool();
    void createCommandBuffer();
    void createSyncObjects();

    void createVertexBuffer();
    void createIndexBuffer();

    void recordCommandBuffer(uint32_t imageIndex) const;
    void recreateSwapchain();
    void cleanupSwapchain();

    VkSurfaceFormatKHR chooseSwapSurfaceFormat() const;
    VkPresentModeKHR chooseSwapPresentMode() const;
    VkExtent2D chooseSwapExtent(GLFWwindow* window) const;

    VkShaderModule createShaderModule(const std::string& filePath) const;
    void transitionImageLayout(
        uint32_t imageIndex,
        VkImageLayout oldLayout,
        VkImageLayout newLayout,
        VkAccessFlags2 srcAccessMask,
        VkAccessFlags2 dstAccessMask,
        VkPipelineStageFlags2 srcStageMask,
        VkPipelineStageFlags2 dstStageMask
    ) const;
    uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties) const;
    std::pair<VkBuffer, VkDeviceMemory> createBuffer(VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties) const;
    void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size) const;

    std::unique_ptr<VulkanDebug> m_debug;
    GLFWwindow* m_window = nullptr;

    VkInstance m_instance = VK_NULL_HANDLE;
    VkPhysicalDevice m_physicalDevice = VK_NULL_HANDLE;
    VkDevice m_device = VK_NULL_HANDLE;
    VkQueue m_graphicsQueue = VK_NULL_HANDLE;

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

    VkCommandPool m_commandPool = VK_NULL_HANDLE;
    std::vector<VkCommandBuffer> m_commandBuffers;
    std::vector<VkSemaphore> m_imageAvailableSemaphores;
    std::vector<VkSemaphore> m_renderCompleteSemaphores;
    std::vector<VkFence> m_drawFences;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
    uint32_t m_currentFrame = 0;

    VkBuffer m_vertexBuffer = VK_NULL_HANDLE;
    VkDeviceMemory m_vertexBufferMemory = VK_NULL_HANDLE;
    VkBuffer m_indexBuffer = VK_NULL_HANDLE;
    VkDeviceMemory m_indexBufferMemory = VK_NULL_HANDLE;

#ifdef NDEBUG
    const bool m_enableValidationLayers = false;
#else
    const bool m_enableValidationLayers = true;
#endif
};


#endif //CREATE_INSTANCE_VULKANENGINE_H
