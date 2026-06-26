#include "Object.h"

#include <chrono>
#include <stdexcept>

#include <glm/gtc/matrix_transform.hpp>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include "VulkanHelper.h"

Object::Object(VulkanContext* context)
    : m_context(context)
{
    createTextureImage();
    createTextureImageView();
    createTextureSampler();
    createVertexBuffer();
    createIndexBuffer();
    createUniformBuffers();

    createDescriptorSetLayout();
    createDescriptorSets();
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

    vkDestroyImage(m_context->device(), m_textureImage, nullptr);
    vkFreeMemory(m_context->device(), m_textureImageMemory, nullptr);

    vkDestroyDescriptorSetLayout(m_context->device(), m_descriptorSetLayout, nullptr);
}

void Object::beforeRender(uint32_t currentImage, float windowWidth, float windowHeight) const
{
    updateUniformBuffer(currentImage, windowWidth, windowHeight);
}

void Object::render(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, uint32_t imageIndex) const
{
    vkCmdBindDescriptorSets(
        commandBuffer,
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        pipelineLayout,
        0,
        1,
        &m_descriptorSets[imageIndex],
        0,
        nullptr
    );

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
    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    copyBuffer(commandBuffer, stagingBuffer, vertexBuffer, bufferSize);
    m_context->endSingleTimeCommands(commandBuffer);

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
    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    copyBuffer(commandBuffer, stagingBuffer, indexBuffer, bufferSize);
    m_context->endSingleTimeCommands(commandBuffer);

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

void Object::createTextureImage()
{
    int width, height, channels;
    stbi_uc *pixels = stbi_load("resources/square.png", &width, &height, &channels, STBI_rgb_alpha);
    if (!pixels)
    {
        throw std::runtime_error("Failed to load texture image");
    }

    VkDeviceSize bufSize = width * height * 4;
    auto [stagingBuffer, stagingBufferMemory] = createBuffer(
        bufSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );

    void *data;
    vkMapMemory(m_context->device(), stagingBufferMemory, 0, bufSize, 0, &data);
    memcpy(data, pixels, bufSize);
    vkUnmapMemory(m_context->device(), stagingBufferMemory);

    stbi_image_free(pixels);

    VkImageCreateInfo imageInfo = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .imageType = VK_IMAGE_TYPE_2D,
        .format = VK_FORMAT_R8G8B8A8_SRGB,
        .extent = {
            .width = static_cast<uint32_t>(width),
            .height = static_cast<uint32_t>(height),
            .depth = 1
        },
        .mipLevels = 1,
        .arrayLayers = 1,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED
    };
    VkResult r = vkCreateImage(m_context->device(), &imageInfo, nullptr, &m_textureImage);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create texture image");
    }
    VkMemoryRequirements memRequirements;
    vkGetImageMemoryRequirements(m_context->device(), m_textureImage, &memRequirements);
    VkMemoryAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = memRequirements.size,
        .memoryTypeIndex = findMemoryType(memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
    };
    r = vkAllocateMemory(m_context->device(), &allocInfo, nullptr, &m_textureImageMemory);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to allocate texture image memory");
    }

    vkBindImageMemory(m_context->device(), m_textureImage, m_textureImageMemory, 0);

    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    transitionImageLayout(
        commandBuffer,
        m_textureImage,
        VK_IMAGE_LAYOUT_UNDEFINED,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        VK_ACCESS_2_MEMORY_WRITE_BIT,
        VK_ACCESS_2_TRANSFER_WRITE_BIT,
        VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT,
        VK_PIPELINE_STAGE_2_TRANSFER_BIT
    );
    copyBufferToImage(commandBuffer, stagingBuffer, m_textureImage, width, height);
    transitionImageLayout(
        commandBuffer,
        m_textureImage,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
        VK_ACCESS_2_TRANSFER_WRITE_BIT,
        VK_ACCESS_2_SHADER_READ_BIT,
        VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT
    );
    m_context->endSingleTimeCommands(commandBuffer);
}

void Object::createTextureImageView()
{
    VkImageViewCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .image = m_textureImage,
        .viewType = VK_IMAGE_VIEW_TYPE_2D,
        .format = VK_FORMAT_R8G8B8A8_SRGB,
        .components = {
            .r = VK_COMPONENT_SWIZZLE_IDENTITY,
            .g = VK_COMPONENT_SWIZZLE_IDENTITY,
            .b = VK_COMPONENT_SWIZZLE_IDENTITY,
            .a = VK_COMPONENT_SWIZZLE_IDENTITY,
        },
        .subresourceRange = {
            .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .baseMipLevel = 0,
            .levelCount = 1,
            .baseArrayLayer = 0,
            .layerCount = 1
        }
    };
    VkResult r = vkCreateImageView(m_context->device(), &createInfo, nullptr, &m_textureImageView);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("failed to create image view");
    }
}

void Object::createTextureSampler()
{
    VkPhysicalDeviceProperties props;
    vkGetPhysicalDeviceProperties(m_context->physicalDevice(), &props);

    VkSamplerCreateInfo samplerInfo = {
        .sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        .magFilter = VK_FILTER_LINEAR,
        .minFilter = VK_FILTER_LINEAR,
        .mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR,
        .addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .mipLodBias = 0.0f,
        .anisotropyEnable = VK_TRUE,
        .maxAnisotropy = props.limits.maxSamplerAnisotropy,
        .compareEnable = VK_FALSE,
        .compareOp = VK_COMPARE_OP_ALWAYS,
        .minLod = 0.0f,
        .maxLod = 0.0f,
        .borderColor = VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
        .unnormalizedCoordinates = VK_FALSE,
    };
    VkResult r = vkCreateSampler(m_context->device(), &samplerInfo, nullptr, &m_textureSampler);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("failed to create texture sampler");
    }
}

void Object::createDescriptorSetLayout()
{
    std::array bindings = {
        VkDescriptorSetLayoutBinding{
            .binding = 0,
            .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            .descriptorCount = 1,
            .stageFlags = VK_SHADER_STAGE_VERTEX_BIT
        },
        VkDescriptorSetLayoutBinding{
            .binding = 1,
            .descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            .descriptorCount = 1,
            .stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT
        }
    };
    VkDescriptorSetLayoutCreateInfo layoutInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .bindingCount = bindings.size(),
        .pBindings = bindings.data()
    };
    VkResult r = vkCreateDescriptorSetLayout(m_context->device(), &layoutInfo, nullptr, &m_descriptorSetLayout);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create descriptor set layout");
    }
}

void Object::createDescriptorSets()
{
    std::vector layouts(MAX_FRAMES_IN_FLIGHT, m_descriptorSetLayout);
    VkDescriptorSetAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool = m_context->descriptorPool(),
        .descriptorSetCount = static_cast<uint32_t>(MAX_FRAMES_IN_FLIGHT),
        .pSetLayouts = layouts.data()
    };
    m_descriptorSets.resize(MAX_FRAMES_IN_FLIGHT);
    VkResult r = vkAllocateDescriptorSets(m_context->device(), &allocInfo, m_descriptorSets.data());
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to allocate descriptor sets");
    }

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        VkDescriptorBufferInfo bufferInfo = {
            .buffer = m_uniformBuffers[i],
            .offset = 0,
            .range = sizeof(UniformBufferObject)
        };
        VkDescriptorImageInfo imageInfo = {
            .sampler = m_textureSampler,
            .imageView = m_textureImageView,
            .imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        };
        std::array descriptorWrite = {
            VkWriteDescriptorSet{
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = m_descriptorSets[i],
                .dstBinding = 0,
                .dstArrayElement = 0,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                .pBufferInfo = &bufferInfo
            },
            VkWriteDescriptorSet{
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = m_descriptorSets[i],
                .dstBinding = 1,
                .dstArrayElement = 0,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                .pImageInfo = &imageInfo
            }
        };
        vkUpdateDescriptorSets(m_context->device(), descriptorWrite.size(), descriptorWrite.data(), 0, nullptr);
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

void Object::copyBuffer(
    VkCommandBuffer commandBuffer,
    VkBuffer srcBuffer,
    VkBuffer dstBuffer,
    VkDeviceSize size
) const
{
    VkBufferCopy copyRegion = {
        .srcOffset = 0,
        .dstOffset = 0,
        .size = size
    };
    vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, &copyRegion);
}

void Object::copyBufferToImage(
    VkCommandBuffer commandBuffer,
    VkBuffer buffer,
    VkImage image,
    uint32_t width,
    uint32_t height
) const
{
    VkBufferImageCopy region = {
        .bufferOffset = 0,
        .bufferRowLength = 0,
        .bufferImageHeight = 0,
        .imageSubresource = {
            .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .mipLevel = 0,
            .baseArrayLayer = 0,
            .layerCount = 1
        },
        .imageOffset = {0, 0, 0},
        .imageExtent = {
            .width = width,
            .height = height,
            .depth = 1
        }
    };
    vkCmdCopyBufferToImage(
        commandBuffer,
        buffer,
        image,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        1,
        &region
    );
}
