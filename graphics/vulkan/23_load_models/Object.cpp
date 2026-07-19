#include "Object.h"

#include <chrono>
#include <stdexcept>
#include <string>
#include <map>
#include <filesystem>
#include <iostream>

#include <glm/gtc/matrix_transform.hpp>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>
#define TINYOBJLOADER_IMPLEMENTATION
#include <tiny_obj_loader.h>

#include "VulkanHelper.h"
#include "VulkanBuffer.h"

Object::Object(VulkanContext* context)
    : m_context(context)
{
    namespace fs = std::filesystem;

    if (!fs::exists(fs::path(TEXTURE_PATH)))
    {
        std::cerr << "[ERROR] File not found: " << TEXTURE_PATH << std::endl;
        exit(1);
    }

    createTextureImage();
    createTextureSampler();

    if (!fs::exists(fs::path(MODEL_PATH)))
    {
        std::cerr << "[ERROR] File not found: " << MODEL_PATH << std::endl;
        exit(1);
    }

    loadModel();
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
        m_uniformBuffers[i].destroy(m_context);
    }
    m_vertexBuffer.destroy(m_context);
    m_indexBuffer.destroy(m_context);

    m_textureImage.destroy(m_context);
    vkDestroySampler(m_context->device(), m_textureSampler, nullptr);

    vkDestroyDescriptorSetLayout(m_context->device(), m_descriptorSetLayout, nullptr);
}

void Object::beforeRender(uint32_t currentFrame, float windowWidth, float windowHeight) const
{
    updateUniformBuffer(currentFrame, windowWidth, windowHeight);
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

void Object::createUniformBuffers()
{
    VkDeviceSize bufferSize = sizeof(UniformBufferObject);

    m_uniformBuffers.resize(MAX_FRAMES_IN_FLIGHT);
    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        m_uniformBuffers[i].create(
            m_context,
            bufferSize,
            VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
        );
    }
}

void Object::createTextureImage()
{
    int width, height, channels;
    stbi_uc *pixels = stbi_load(TEXTURE_PATH.c_str(), &width, &height, &channels, STBI_rgb_alpha);
    if (!pixels)
    {
        throw std::runtime_error("Failed to load texture image");
    }

    VkDeviceSize bufSize = width * height * 4;
    VulkanMappedBuffer<void> stagingBuffer;
    stagingBuffer.create(
        m_context,
        bufSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    );
    memcpy(stagingBuffer.mappedData, pixels, bufSize);
    stagingBuffer.unmap(m_context);

    stbi_image_free(pixels);

    m_textureImage.create(
        m_context,
        width, height,
        VK_FORMAT_R8G8B8A8_SRGB,
        VK_IMAGE_TILING_OPTIMAL,
        VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        VK_IMAGE_ASPECT_COLOR_BIT
    );

    VkCommandBuffer commandBuffer = m_context->beginSingleTimeCommands();
    transitionImageLayout(
        commandBuffer,
        m_textureImage.image,
        VK_IMAGE_ASPECT_COLOR_BIT,
        VK_IMAGE_LAYOUT_UNDEFINED,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        VK_ACCESS_2_MEMORY_WRITE_BIT,
        VK_ACCESS_2_TRANSFER_WRITE_BIT,
        VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT,
        VK_PIPELINE_STAGE_2_TRANSFER_BIT
    );
    m_textureImage.copyFrom(commandBuffer, stagingBuffer);
    transitionImageLayout(
        commandBuffer,
        m_textureImage.image,
        VK_IMAGE_ASPECT_COLOR_BIT,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
        VK_ACCESS_2_TRANSFER_WRITE_BIT,
        VK_ACCESS_2_SHADER_READ_BIT,
        VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT
    );
    m_context->endSingleTimeCommands(commandBuffer);

    stagingBuffer.destroy(m_context);
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

void Object::loadModel()
{
    tinyobj::attrib_t attrib;
    std::vector<tinyobj::shape_t> shapes;
    std::vector<tinyobj::material_t> materials;
    std::string warn, err;

    bool result = tinyobj::LoadObj(&attrib, &shapes, &materials, &warn, &err, MODEL_PATH.c_str());
    if (!result)
    {
        throw std::runtime_error(warn + err);
    }

    std::map<Vertex, uint16_t> uniqueVertices;

    for (const auto& shape : shapes)
    {
        for (const auto& index : shape.mesh.indices)
        {
            Vertex vertex{
                .pos = {
                    attrib.vertices[3 * index.vertex_index + 0],
                    attrib.vertices[3 * index.vertex_index + 1],
                    attrib.vertices[3 * index.vertex_index + 2],
                },
                .uv = {
                    attrib.texcoords[2 * index.texcoord_index + 0],
                    1.0f - attrib.texcoords[2 * index.texcoord_index + 1],
                }
            };

            if (!uniqueVertices.contains(vertex))
            {
                uniqueVertices[vertex] = vertices.size();
                vertices.push_back(vertex);
            }

            indices.push_back(uniqueVertices[vertex]);
        }
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
            .buffer = m_uniformBuffers[i].buffer,
            .offset = 0,
            .range = sizeof(UniformBufferObject)
        };
        VkDescriptorImageInfo imageInfo = {
            .sampler = m_textureSampler,
            .imageView = m_textureImage.imageView,
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

void Object::updateUniformBuffer(uint32_t currentFrame, float windowWidth, float windowHeight) const
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

    memcpy(m_uniformBuffers[currentFrame].mappedData, &ubo, sizeof(ubo));
}
