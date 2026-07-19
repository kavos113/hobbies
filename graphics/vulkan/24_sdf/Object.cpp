#include "Object.h"

#include <chrono>
#include <stdexcept>

#include <glm/gtc/matrix_transform.hpp>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include "VulkanHelper.h"
#include "VulkanBuffer.h"

Object::Object(VulkanContext* context)
    : m_context(context)
{
    createVertexBuffer();
    createIndexBuffer();
}

Object::~Object()
{
    m_vertexBuffer.destroy(m_context);
    m_indexBuffer.destroy(m_context);
}

void Object::beforeRender(uint32_t currentFrame, float windowWidth, float windowHeight) const
{
}

void Object::render(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t imageIndex) const
{
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, &m_vertexBuffer.buffer, offsets);
    vkCmdBindIndexBuffer(commandBuffer, m_indexBuffer.buffer, 0, VK_INDEX_TYPE_UINT16);

    vkCmdDrawIndexed(commandBuffer, static_cast<uint32_t>(indices.size()), 1, 0, 0, 0);
}

void Object::createVertexBuffer()
{
    VkDeviceSize bufferSize = sizeof(vertices[0]) * vertices.size();

    VulkanMappedBuffer<void> stagingBuffer;
    stagingBuffer.create(
        m_context,
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );
    memcpy(stagingBuffer.mappedData, vertices.data(), bufferSize);
    stagingBuffer.unmap(m_context);

    m_vertexBuffer.create(
        m_context,
        bufferSize,
        VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    );
    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    m_vertexBuffer.copyFrom(commandBuffer, stagingBuffer);
    m_context->endSingleTimeCommands(commandBuffer);

    stagingBuffer.destroy(m_context);
}

void Object::createIndexBuffer()
{
    VkDeviceSize bufferSize = sizeof(indices[0]) * indices.size();

    VulkanMappedBuffer<void> stagingBuffer;
    stagingBuffer.create(
        m_context,
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );
    memcpy(stagingBuffer.mappedData, indices.data(), bufferSize);
    stagingBuffer.unmap(m_context);

    m_indexBuffer.create(
        m_context,
        bufferSize,
        VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    );
    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    m_indexBuffer.copyFrom(commandBuffer, stagingBuffer);
    m_context->endSingleTimeCommands(commandBuffer);

    stagingBuffer.destroy(m_context);
}
