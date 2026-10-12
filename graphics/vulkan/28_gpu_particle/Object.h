#ifndef REFACTOR_OBJECT_H
#define REFACTOR_OBJECT_H

#include <array>
#include <chrono>
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

    void beforeRender(uint32_t currentFrame, float windowWidth, float windowHeight);
    void render(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t frameIndex) const;
    void dispatch(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t frameIndex) const;

    VkDescriptorSetLayout computeDescriptorSetLayout() const
    {
        return m_computeDescriptorSetLayout;
    }

    struct Particle
    {
        glm::vec2 position;
        glm::vec2 velocity;
        glm::vec4 color;

        static VkVertexInputBindingDescription bindingDescription()
        {
            return {
                .binding = 0,
                .stride = sizeof(Particle),
                .inputRate = VK_VERTEX_INPUT_RATE_VERTEX,
            };
        }

        static std::array<VkVertexInputAttributeDescription, 2> attributeDescriptions()
        {
            return {
                VkVertexInputAttributeDescription{
                    .location = 0,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32_SFLOAT,
                    .offset = offsetof(Particle, position)
                },
                VkVertexInputAttributeDescription{
                    .location = 1,
                    .binding = 0,
                    .format = VK_FORMAT_R32G32B32A32_SFLOAT,
                    .offset = offsetof(Particle, color)
                }
            };
        }
    };

private:
    struct UniformBufferObject
    {
        float deltaTime = 1.0f;
    };

private:
    void createShaderStorageBuffers();

    void createUniformBuffers();

    void createComputeDescriptorSetLayout();
    void createComputeDescriptorSets();

    void updateUniformBuffer(uint32_t currentFrame);

    VulkanContext *m_context;

    VkDescriptorSetLayout m_computeDescriptorSetLayout = VK_NULL_HANDLE;
    std::vector<VkDescriptorSet> m_computeDescriptorSets;

    std::vector<VulkanMappedBuffer<void>> m_uniformBuffers;
    std::vector<VulkanBuffer> m_shaderStorageBuffers;

    static constexpr int MAX_FRAMES_IN_FLIGHT = 2;
    static constexpr int PARTICLE_COUNT = 8192;

    std::chrono::time_point<std::chrono::steady_clock> m_lastTime;
};


#endif //REFACTOR_OBJECT_H
