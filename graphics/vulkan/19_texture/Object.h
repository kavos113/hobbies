#ifndef REFACTOR_OBJECT_H
#define REFACTOR_OBJECT_H

#include <array>
#include <vector>
#include <utility>

#include <vulkan/vulkan.h>
#include <glm/glm.hpp>

#include "VulkanContext.h"

class Object
{
public:
    Object(VulkanContext *context);
    ~Object();

    void beforeRender(uint32_t currentImage, float windowWidth, float windowHeight) const;
    void render(VkCommandBuffer commandBuffer) const;

    VkDescriptorBufferInfo uniformBufferInfo(uint32_t currentImage) const
    {
        return {
            .buffer = m_uniformBuffers[currentImage],
            .offset = 0,
            .range = sizeof(UniformBufferObject)
        };
    }

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

private:
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

    struct UniformBufferObject
    {
        glm::mat4 model;
        glm::mat4 view;
        glm::mat4 proj;
    };

private:
    void createVertexBuffer();
    void createIndexBuffer();
    void createUniformBuffers();
    void createTextureImages();

    void updateUniformBuffer(uint32_t currentImage, float windowWidth, float windowHeight) const;

    uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties) const;
    std::pair<VkBuffer, VkDeviceMemory> createBuffer(VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties) const;
    void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size) const;

    VulkanContext *m_context;

    VkBuffer m_vertexBuffer = VK_NULL_HANDLE;
    VkDeviceMemory m_vertexBufferMemory = VK_NULL_HANDLE;
    VkBuffer m_indexBuffer = VK_NULL_HANDLE;
    VkDeviceMemory m_indexBufferMemory = VK_NULL_HANDLE;
    std::vector<VkBuffer> m_uniformBuffers;
    std::vector<VkDeviceMemory> m_uniformBuffersMemory;
    std::vector<void *> m_uniformBuffersMapped;
    VkImage m_textureImage;
    VkDeviceMemory m_textureImageMemory;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
};


#endif //REFACTOR_OBJECT_H
