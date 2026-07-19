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
        glm::vec2 uv;

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
                    .format = VK_FORMAT_R32G32B32_SFLOAT,
                    .offset = offsetof(Vertex, pos)
                },
                VkVertexInputAttributeDescription{
                    .location = 1,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32_SFLOAT,
                    .offset = offsetof(Vertex, uv)
                },
            };
        }

        bool operator<(const Vertex& other) const
        {
            return pos.x < other.pos.x ||
                   (pos.x == other.pos.x && pos.y < other.pos.y) ||
                   (pos.x == other.pos.x && pos.y == other.pos.y && pos.z < other.pos.z) ||
                   (pos.x == other.pos.x && pos.y == other.pos.y && pos.z == other.pos.z && uv.x < other.uv.x) ||
                   (pos.x == other.pos.x && pos.y == other.pos.y && pos.z == other.pos.z && uv.x == other.uv.x && uv.y < other.uv.y);
        }
    };

private:
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
    void loadModel();

    void createDescriptorSetLayout();
    void createDescriptorSets();

    void updateUniformBuffer(uint32_t currentFrame, float windowWidth, float windowHeight) const;

    VulkanContext *m_context;

    VkDescriptorSetLayout m_descriptorSetLayout = VK_NULL_HANDLE;
    std::vector<VkDescriptorSet> m_descriptorSets;

    VulkanBuffer m_vertexBuffer;
    VulkanBuffer m_indexBuffer;
    std::vector<VulkanMappedBuffer<void>> m_uniformBuffers;

    std::vector<Vertex> vertices;
    std::vector<uint16_t> indices;

    VulkanImage m_textureImage;
    VkSampler m_textureSampler = VK_NULL_HANDLE;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
    const std::string TEXTURE_PATH = "resources/models/textures/utility_box_02_diff_1k.jpg";
    const std::string MODEL_PATH = "resources/models/utility_box.obj";
};


#endif //REFACTOR_OBJECT_H
