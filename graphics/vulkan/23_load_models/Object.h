#ifndef REFACTOR_OBJECT_H
#define REFACTOR_OBJECT_H

#include <array>
#include <vector>
#include <utility>

#include <vulkan/vulkan.h>
#include <glm/glm.hpp>

#include "VulkanContext.h"
#include "VulkanBuffer.h"

class Object
{
public:
    Object(VulkanContext *context);
    ~Object();

    void beforeRender(uint32_t currentFrame, float windowWidth, float windowHeight) const;
    void render(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t imageIndex) const;

    VkDescriptorSetLayout descriptorSetLayout() const
    {
        return m_descriptorSetLayout;
    }

    struct Vertex
    {
        glm::vec3 pos;
        glm::vec3 color;
        glm::vec2 uv;

        static VkVertexInputBindingDescription getBindingDescription()
        {
            return {
                .binding = 0,
                .stride = sizeof(Vertex),
                .inputRate = VK_VERTEX_INPUT_RATE_VERTEX
            };
        }

        static std::array<VkVertexInputAttributeDescription, 3> getAttributeDescriptions()
        {
            return {
                VkVertexInputAttributeDescription{
                    .location = 0,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32B32_SFLOAT,
                    .offset = offsetof(Vertex, pos)
                },
                VkVertexInputAttributeDescription{
                    .location = 1,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32B32_SFLOAT,
                    .offset = offsetof(Vertex, color)
                },
                VkVertexInputAttributeDescription{
                    .location = 2,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32_SFLOAT,
                    .offset = offsetof(Vertex, uv)
                },
            };
        }
    };

private:
    const std::vector<Vertex> vertices = {
        {{-0.5f, -0.5f, 0.0f}, {1.0f, 0.0f, 0.0f}, {1.0f, 0.0f}},
        {{0.5f, -0.5f, 0.0f}, {0.0f, 1.0f, 0.0f}, {0.0f, 0.0f}},
        {{0.5f, 0.5f, 0.0f}, {0.0f, 0.0f, 1.0f}, {0.0f, 1.0f}},
        {{-0.5f, 0.5f, 0.0f}, {1.0f, 1.0f, 1.0f}, {1.0f, 1.0f}},

    {{-0.5f, -0.5f, -0.5f}, {1.0f, 0.0f, 0.0f}, {1.0f, 0.0f}},
    {{0.5f, -0.5f, -0.5f}, {0.0f, 1.0f, 0.0f}, {0.0f, 0.0f}},
    {{0.5f, 0.5f, -0.5f}, {0.0f, 0.0f, 1.0f}, {0.0f, 1.0f}},
    {{-0.5f, 0.5f, -0.5f}, {1.0f, 1.0f, 1.0f}, {1.0f, 1.0f}},
    };
    const std::vector<uint16_t> indices = {
        0, 1, 2,  2, 3, 0,
        4, 5, 6,  6, 7, 4
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

    void createTextureImage();
    void createTextureSampler();

    void createDescriptorSetLayout();
    void createDescriptorSets();

    void updateUniformBuffer(uint32_t currentFrame, float windowWidth, float windowHeight) const;

    VulkanContext *m_context;

    VkDescriptorSetLayout m_descriptorSetLayout = VK_NULL_HANDLE;
    std::vector<VkDescriptorSet> m_descriptorSets;

    VulkanBuffer m_vertexBuffer;
    VulkanBuffer m_indexBuffer;
    std::vector<VulkanMappedBuffer<void>> m_uniformBuffers;

    VulkanImage m_textureImage;
    VkSampler m_textureSampler = VK_NULL_HANDLE;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
};


#endif //REFACTOR_OBJECT_H
