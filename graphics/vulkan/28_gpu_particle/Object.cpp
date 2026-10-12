#include "Object.h"

#include <chrono>
#include <stdexcept>
#include <random>

#include <glm/gtc/matrix_transform.hpp>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>
#define TINYOBJLOADER_IMPLEMENTATION
#include <numbers>
#include <tiny_obj_loader.h>

#include "VulkanHelper.h"
#include "VulkanBuffer.h"

Object::Object(VulkanContext* context)
    : m_context(context)
{
    createShaderStorageBuffers();

    createUniformBuffers();

    createComputeDescriptorSetLayout();
    createComputeDescriptorSets();

    m_lastTime = std::chrono::steady_clock::now();
}

Object::~Object()
{
    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        m_uniformBuffers[i].destroy(m_context);
        m_shaderStorageBuffers[i].destroy(m_context);
    }

    vkDestroyDescriptorSetLayout(m_context->device(), m_computeDescriptorSetLayout, nullptr);
}

void Object::beforeRender(uint32_t currentFrame, float windowWidth, float windowHeight)
{
    updateUniformBuffer(currentFrame);
}

void Object::render(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t frameIndex) const
{
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, &m_shaderStorageBuffers[frameIndex].buffer, offsets);

    vkCmdDraw(commandBuffer, PARTICLE_COUNT, 1, 0, 0);
}

void Object::dispatch(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t frameIndex) const
{
    vkCmdBindDescriptorSets(
        commandBuffer,
        VK_PIPELINE_BIND_POINT_COMPUTE,
        pipelineLayout,
        0,
        1,
        &m_computeDescriptorSets[frameIndex],
        0,
        nullptr
    );

    vkCmdDispatch(commandBuffer, PARTICLE_COUNT / 256, 1, 1);
}

void Object::createShaderStorageBuffers()
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<float> dist(0.0f, 1.0f);

    std::vector<Particle> particles(PARTICLE_COUNT);
    for (auto &p : particles)
    {
        float r = 0.25f * sqrtf(dist(gen));
        float theta = dist(gen) * 2.0f * std::numbers::pi;

        float x = r * cosf(theta);
        float y = r * sinf(theta);

        p.position = glm::vec2(x, y);
        p.velocity = glm::normalize(glm::vec2(x, y)) * 0.00025f;
        p.color = glm::vec4(dist(gen), dist(gen), dist(gen), 1.0f);
    }

    VkDeviceSize bufSize = sizeof(Particle) * PARTICLE_COUNT;

    VulkanMappedBuffer<void> stagingBuffer;
    stagingBuffer.create(m_context, bufSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT);
    memcpy(stagingBuffer.mappedData, particles.data(), bufSize);

    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    m_shaderStorageBuffers.resize(2);
    for (int i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        m_shaderStorageBuffers[i].create(
            m_context,
            bufSize,
            VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT
        );
        m_shaderStorageBuffers[i].copyFrom(commandBuffer, stagingBuffer);
    }
    m_context->endSingleTimeCommands(commandBuffer);

    stagingBuffer.destroy(m_context);
}

void Object::createUniformBuffers()
{
    VkDeviceSize bufferSize = sizeof(UniformBufferObject);

    m_uniformBuffers.resize(MAX_FRAMES_IN_FLIGHT);
    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        m_uniformBuffers[i].create(
            m_context,
            bufferSize,
            VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        );
    }
}

void Object::createComputeDescriptorSetLayout()
{
    std::array bindings = {
        VkDescriptorSetLayoutBinding{
            .binding = 0,
            .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            .descriptorCount = 1,
            .stageFlags = VK_SHADER_STAGE_COMPUTE_BIT
        },
        VkDescriptorSetLayoutBinding{
            .binding = 1,
            .descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            .descriptorCount = 1,
            .stageFlags = VK_SHADER_STAGE_COMPUTE_BIT
        },
        VkDescriptorSetLayoutBinding{
            .binding = 2,
            .descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            .descriptorCount = 1,
            .stageFlags = VK_SHADER_STAGE_COMPUTE_BIT
        },
    };

    VkDescriptorSetLayoutCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .bindingCount = bindings.size(),
        .pBindings = bindings.data()
    };
    VkResult r = vkCreateDescriptorSetLayout(m_context->device(), &createInfo, nullptr, &m_computeDescriptorSetLayout);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("failed to create compute descriptor set layout");
    }
}

void Object::createComputeDescriptorSets()
{
    std::vector layouts(MAX_FRAMES_IN_FLIGHT, m_computeDescriptorSetLayout);
    VkDescriptorSetAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool = m_context->descriptorPool(),
        .descriptorSetCount = static_cast<uint32_t>(MAX_FRAMES_IN_FLIGHT),
        .pSetLayouts = layouts.data()
    };
    m_computeDescriptorSets.resize(MAX_FRAMES_IN_FLIGHT);
    VkResult r = vkAllocateDescriptorSets(m_context->device(), &allocInfo, m_computeDescriptorSets.data());
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("failed to allocate compute descriptor sets");
    }

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        // frame 0:  | ubo | buf1 | buf0 |
        // frame 1:  | ubo | buf0 | buf1 |
        VkDescriptorBufferInfo bufferInfo = {
            .buffer = m_uniformBuffers[i].buffer,
            .offset = 0,
            .range = sizeof(UniformBufferObject)
        };
        VkDescriptorBufferInfo storageBufferInfoLast = {
            .buffer = m_shaderStorageBuffers[(i - 1) % MAX_FRAMES_IN_FLIGHT].buffer,
            .offset = 0,
            .range = sizeof(Particle) * PARTICLE_COUNT
        };
        VkDescriptorBufferInfo storageBufferInfoCurrent = {
            .buffer = m_shaderStorageBuffers[i].buffer,
            .offset = 0,
            .range = sizeof(Particle) * PARTICLE_COUNT
        };
        std::array descriptorWrite = {
            VkWriteDescriptorSet{
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = m_computeDescriptorSets[i],
                .dstBinding = 0,
                .dstArrayElement = 0,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                .pBufferInfo = &bufferInfo
            },
            VkWriteDescriptorSet{
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = m_computeDescriptorSets[i],
                .dstBinding = 1,
                .dstArrayElement = 0,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                .pBufferInfo = &storageBufferInfoLast
            },
            VkWriteDescriptorSet{
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = m_computeDescriptorSets[i],
                .dstBinding = 2,
                .dstArrayElement = 0,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                .pBufferInfo = &storageBufferInfoCurrent
            },
        };
        vkUpdateDescriptorSets(m_context->device(), descriptorWrite.size(), descriptorWrite.data(), 0, nullptr);
    }
}

void Object::updateUniformBuffer(uint32_t currentFrame)
{
    UniformBufferObject ubo{};

    auto now = std::chrono::steady_clock::now();
    uint64_t deltaMs = std::chrono::duration_cast<std::chrono::milliseconds>(now - m_lastTime).count();
    ubo.deltaTime = static_cast<float>(deltaMs) / 1000.0f;
    m_lastTime = now;

    memcpy(m_uniformBuffers[currentFrame].mappedData, &ubo, sizeof(ubo));
}
