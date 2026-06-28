#include "Object.h"

#include <chrono>
#include <stdexcept>

#include <glm/gtc/matrix_transform.hpp>

Object::Object(VulkanContext* context)
    : m_context(context)
{
    createVertexBuffer();
    createIndexBuffer();
    createUniformBuffers();
}

Object::~Object()
{
    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        vkDestroyBuffer(m_context->device(), m_uniformBuffers[i], nullptr);
        vkFreeMemory(m_context->device(), m_uniformBuffersMemory[i], nullptr);
    }
    vkDestroyBuffer(m_context->device(), m_vertexBuffer, nullptr);
    vkFreeMemory(m_context->device(), m_vertexBufferMemory, nullptr);
    vkDestroyBuffer(m_context->device(), m_indexBuffer, nullptr);
    vkFreeMemory(m_context->device(), m_indexBufferMemory, nullptr);
}

void Object::beforeRender(uint32_t currentImage, float windowWidth, float windowHeight) const
{
    updateUniformBuffer(currentImage, windowWidth, windowHeight);
}

void Object::render(VkCommandBuffer commandBuffer) const
{
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, &m_vertexBuffer, offsets);
    vkCmdBindIndexBuffer(commandBuffer, m_indexBuffer, 0, VK_INDEX_TYPE_UINT16);

    vkCmdDrawIndexed(commandBuffer, static_cast<uint32_t>(indices.size()), 1, 0, 0, 0);
}

void Object::createVertexBuffer()
{
    VkDeviceSize bufferSize = sizeof(vertices[0]) * vertices.size();

    auto [stagingBuffer, stagingBufferMemory] = createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );

    void *data;
    vkMapMemory(m_context->device(), stagingBufferMemory, 0, bufferSize, 0, &data);
    memcpy(data, vertices.data(), bufferSize);
    vkUnmapMemory(m_context->device(), stagingBufferMemory);

    auto [vertexBuffer, vertexBufferMemory] = createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    );
    copyBuffer(stagingBuffer, vertexBuffer, bufferSize);

    m_vertexBuffer = vertexBuffer;
    m_vertexBufferMemory = vertexBufferMemory;

    vkDestroyBuffer(m_context->device(), stagingBuffer, nullptr);
    vkFreeMemory(m_context->device(), stagingBufferMemory, nullptr);
}

void Object::createIndexBuffer()
{
    VkDeviceSize bufferSize = sizeof(indices[0]) * indices.size();

    auto [stagingBuffer, stagingBufferMemory] = createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );

    void *data;
    vkMapMemory(m_context->device(), stagingBufferMemory, 0, bufferSize, 0, &data);
    memcpy(data, indices.data(), bufferSize);
    vkUnmapMemory(m_context->device(), stagingBufferMemory);

    auto [indexBuffer, indexBufferMemory] = createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    );
    copyBuffer(stagingBuffer, indexBuffer, bufferSize);

    m_indexBuffer = indexBuffer;
    m_indexBufferMemory = indexBufferMemory;

    vkDestroyBuffer(m_context->device(), stagingBuffer, nullptr);
    vkFreeMemory(m_context->device(), stagingBufferMemory, nullptr);
}

void Object::createUniformBuffers()
{
    VkDeviceSize bufferSize = sizeof(UniformBufferObject);

    m_uniformBuffers.resize(MAX_FRAMES_IN_FLIGHT);
    m_uniformBuffersMemory.resize(MAX_FRAMES_IN_FLIGHT);
    m_uniformBuffersMapped.resize(MAX_FRAMES_IN_FLIGHT);

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        auto [buffer, bufferMemory] = createBuffer(
            bufferSize,
            VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
        );
        m_uniformBuffers[i] = buffer;
        m_uniformBuffersMemory[i] = bufferMemory;

        VkResult r = vkMapMemory(m_context->device(), bufferMemory, 0, bufferSize, 0, &m_uniformBuffersMapped[i]);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to map uniform buffer memory");
        }
    }
}

void Object::updateUniformBuffer(uint32_t currentImage, float windowWidth, float windowHeight) const
{
    using namespace std::chrono;

    static time_point<steady_clock> startTime = high_resolution_clock::now();
    time_point<steady_clock> currentTime = high_resolution_clock::now();
    float time = duration<float>(currentTime - startTime).count();

    UniformBufferObject ubo = {};
    ubo.model = glm::rotate(
        glm::mat4(1.0f),
        time * glm::radians(90.0f),
        glm::vec3(0.0f, 0.0f, 1.0f)
    );
    ubo.view = glm::lookAt(
        glm::vec3(2.0f, 2.0f, 2.0f),
        glm::vec3(0.0f, 0.0f, 0.0f),
        glm::vec3(0.0f, 0.0f, 1.0f)
    );
    ubo.proj = glm::perspective(
        glm::radians(45.0f),
        windowWidth / windowHeight,
        0.1f,
        10.0f
    );
    ubo.proj[1][1] *= -1; // Invert Y coordinate for Vulkan

    memcpy(m_uniformBuffersMapped[currentImage], &ubo, sizeof(ubo));
}

uint32_t Object::findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties) const
{
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(m_context->physicalDevice(), &memProperties);

    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
    {
        if (typeFilter & 1 << i && (memProperties.memoryTypes[i].propertyFlags & properties) == properties)
        {
            return i;
        }
    }

    throw std::runtime_error("Failed to find suitable memory type");
}

std::pair<VkBuffer, VkDeviceMemory> Object::createBuffer(
    VkDeviceSize size,
    VkBufferUsageFlags usage,
    VkMemoryPropertyFlags properties
) const
{
    VkBufferCreateInfo bufferInfo = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE
    };
    VkBuffer buffer;
    VkResult r = vkCreateBuffer(m_context->device(), &bufferInfo, nullptr, &buffer);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create buffer");
    }

    VkMemoryRequirements memRequirements;
    vkGetBufferMemoryRequirements(m_context->device(), buffer, &memRequirements);

    VkMemoryAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = memRequirements.size,
        .memoryTypeIndex = findMemoryType(memRequirements.memoryTypeBits, properties)
    };
    VkDeviceMemory bufferMemory;
    r = vkAllocateMemory(m_context->device(), &allocInfo, nullptr, &bufferMemory);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to allocate buffer memory");
    }

    vkBindBufferMemory(m_context->device(), buffer, bufferMemory, 0);

    return {buffer, bufferMemory};
}

void Object::copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size) const
{
    VkCommandBufferAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = m_context->commandPool(),
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1
    };
    VkCommandBuffer commandBuffer;
    vkAllocateCommandBuffers(m_context->device(), &allocInfo, &commandBuffer);

    VkCommandBufferBeginInfo beginInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    };
    vkBeginCommandBuffer(commandBuffer, &beginInfo);

    VkBufferCopy copyRegion = {
        .srcOffset = 0,
        .dstOffset = 0,
        .size = size
    };
    vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, &copyRegion);

    vkEndCommandBuffer(commandBuffer);

    VkSubmitInfo submitInfo = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .commandBufferCount = 1,
        .pCommandBuffers = &commandBuffer
    };
    vkQueueSubmit(m_context->graphicsQueue(), 1, &submitInfo, VK_NULL_HANDLE);
    vkQueueWaitIdle(m_context->graphicsQueue());

    vkFreeCommandBuffers(m_context->device(), m_context->commandPool(), 1, &commandBuffer);
}
