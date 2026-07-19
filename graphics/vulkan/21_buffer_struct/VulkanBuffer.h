#ifndef BUFFER_STRUCT_VULKANBUFFER_H
#define BUFFER_STRUCT_VULKANBUFFER_H

#include <stdexcept>

#include <vulkan/vulkan.h>

#include "VulkanContext.h"

template <typename T>
struct VulkanMappedBuffer
{
    VkBuffer buffer = VK_NULL_HANDLE;
    VkDeviceMemory memory = VK_NULL_HANDLE;
    VkDeviceSize size = 0;
    T *mappedData = nullptr;

    VulkanMappedBuffer() = default;
    VulkanMappedBuffer(const VulkanMappedBuffer&) = delete;
    VulkanMappedBuffer& operator=(const VulkanMappedBuffer&) = delete;
    VulkanMappedBuffer& operator=(VulkanMappedBuffer&&) = delete;

    VulkanMappedBuffer(VulkanMappedBuffer&& other) noexcept
        : buffer(other.buffer), memory(other.memory), size(other.size), mappedData(other.mappedData)
    {
        other.buffer = VK_NULL_HANDLE;
        other.memory = VK_NULL_HANDLE;
        other.mappedData = nullptr;
        other.size = 0;
    }

    void create(
        const VulkanContext *context,
        VkDeviceSize bufSize,
        VkBufferUsageFlags usage,
        VkMemoryPropertyFlags properties
    )
    {
        size = bufSize;

        VkBufferCreateInfo bufferInfo = {
            .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
            .size = size,
            .usage = usage,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE
        };
        VkResult r = vkCreateBuffer(context->device(), &bufferInfo, nullptr, &buffer);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create buffer");
        }

        VkMemoryRequirements memRequirements;
        vkGetBufferMemoryRequirements(context->device(), buffer, &memRequirements);

        VkMemoryAllocateInfo allocInfo = {
            .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
            .allocationSize = memRequirements.size,
            .memoryTypeIndex = findMemoryType(context, memRequirements.memoryTypeBits, properties)
        };
        r = vkAllocateMemory(context->device(), &allocInfo, nullptr, &memory);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to allocate buffer memory");
        }

        vkBindBufferMemory(context->device(), buffer, memory, 0);
    }

    void destroy(const VulkanContext *context)
    {
        if (buffer != VK_NULL_HANDLE)
        {
            vkDestroyBuffer(context->device(), buffer, nullptr);
            buffer = VK_NULL_HANDLE;
        }
        if (memory != VK_NULL_HANDLE)
        {
            vkFreeMemory(context->device(), memory, nullptr);
        }
    }

    bool allocated() const
    {
        return buffer != VK_NULL_HANDLE && memory != VK_NULL_HANDLE;
    }

    bool mapped() const
    {
        return mappedData != nullptr;
    }

    void map(const VulkanContext *context)
    {
        if (mappedData == nullptr)
        {
            vkMapMemory(context->device(), memory, 0, size, 0, &mappedData);
        }
    }

    void unmap(const VulkanContext *context)
    {
        if (mappedData != nullptr)
        {
            vkUnmapMemory(context->device(), memory);
            mappedData = nullptr;
        }
    }

private:
    static uint32_t findMemoryType(const VulkanContext *context, uint32_t typeFilter, VkMemoryPropertyFlags properties)
    {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(context->physicalDevice(), &memProperties);

        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
        {
            if (typeFilter & 1 << i && (memProperties.memoryTypes[i].propertyFlags & properties) == properties)
            {
                return i;
            }
        }

        throw std::runtime_error("Failed to find suitable memory type");
    }
};

struct VulkanBuffer
{
    VkBuffer buffer = VK_NULL_HANDLE;
    VkDeviceMemory memory = VK_NULL_HANDLE;

    VulkanBuffer() = default;
    VulkanBuffer(const VulkanBuffer&) = delete;
    VulkanBuffer& operator=(const VulkanBuffer&) = delete;

    VulkanBuffer(VulkanBuffer&& other) noexcept
        : buffer(other.buffer), memory(other.memory)
    {
        other.buffer = VK_NULL_HANDLE;
        other.memory = VK_NULL_HANDLE;
    }

    VulkanBuffer& operator=(VulkanBuffer&&) = delete;

    void create(
        const VulkanContext *context,
        VkDeviceSize size,
        VkBufferUsageFlags usage,
        VkMemoryPropertyFlags properties
    )
    {
        VkBufferCreateInfo bufferInfo = {
            .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
            .size = size,
            .usage = usage,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE
        };
        VkResult r = vkCreateBuffer(context->device(), &bufferInfo, nullptr, &buffer);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create buffer");
        }

        VkMemoryRequirements memRequirements;
        vkGetBufferMemoryRequirements(context->device(), buffer, &memRequirements);

        VkMemoryAllocateInfo allocInfo = {
            .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
            .allocationSize = memRequirements.size,
            .memoryTypeIndex = findMemoryType(context, memRequirements.memoryTypeBits, properties)
        };
        r = vkAllocateMemory(context->device(), &allocInfo, nullptr, &memory);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to allocate buffer memory");
        }

        vkBindBufferMemory(context->device(), buffer, memory, 0);
    }

    void destroy(const VulkanContext *context)
    {
        if (buffer != VK_NULL_HANDLE)
        {
            vkDestroyBuffer(context->device(), buffer, nullptr);
            buffer = VK_NULL_HANDLE;
        }
        if (memory != VK_NULL_HANDLE)
        {
            vkFreeMemory(context->device(), memory, nullptr);
        }
    }

    bool allocated() const
    {
        return buffer != VK_NULL_HANDLE && memory != VK_NULL_HANDLE;
    }

    template <typename T>
    void copyFrom(VkCommandBuffer commandBuffer, const VulkanMappedBuffer<T>& staging)
    {
        VkBufferCopy copyRegion = {
            .srcOffset = 0,
            .dstOffset = 0,
            .size = staging.size
        };
        vkCmdCopyBuffer(commandBuffer, staging.buffer, buffer, 1, &copyRegion);
    }

private:
    static uint32_t findMemoryType(const VulkanContext *context, uint32_t typeFilter, VkMemoryPropertyFlags properties)
    {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(context->physicalDevice(), &memProperties);

        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
        {
            if (typeFilter & 1 << i && (memProperties.memoryTypes[i].propertyFlags & properties) == properties)
            {
                return i;
            }
        }

        throw std::runtime_error("Failed to find suitable memory type");
    }
};

struct VulkanImage
{
    VkImage image = VK_NULL_HANDLE;
    VkImageView imageView = VK_NULL_HANDLE;
    VkDeviceMemory memory = VK_NULL_HANDLE;
    uint32_t width = 0;
    uint32_t height = 0;

    VulkanImage() = default;
    VulkanImage(const VulkanImage&) = delete;
    VulkanImage& operator=(const VulkanImage&) = delete;
    VulkanImage& operator=(VulkanImage&&) = delete;

    VulkanImage(VulkanImage&& other) noexcept
        : image(other.image), imageView(other.imageView), memory(other.memory), width(other.width), height(other.height)
    {
        other.image = VK_NULL_HANDLE;
        other.imageView = VK_NULL_HANDLE;
        other.memory = nullptr;
        other.width = 0;
        other.height = 0;
    }

    void create(
        const VulkanContext *context,
        uint32_t w,
        uint32_t h,
        VkFormat format,
        VkImageTiling tiling,
        VkImageUsageFlags usage,
        VkMemoryPropertyFlags properties,
        VkImageAspectFlags imageAspect
    )
    {
        width = w;
        height = h;

        VkImageCreateInfo imageInfo = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
            .imageType = VK_IMAGE_TYPE_2D,
            .format = format,
            .extent = {
                .width = width,
                .height = height,
                .depth = 1
            },
            .mipLevels = 1,
            .arrayLayers = 1,
            .samples = VK_SAMPLE_COUNT_1_BIT,
            .tiling = tiling,
            .usage = usage,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
            .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED
        };
        VkResult r = vkCreateImage(context->device(), &imageInfo, nullptr, &image);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create texture image");
        }

        VkMemoryRequirements memRequirements;
        vkGetImageMemoryRequirements(context->device(), image, &memRequirements);
        VkMemoryAllocateInfo allocInfo = {
            .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
            .allocationSize = memRequirements.size,
            .memoryTypeIndex = findMemoryType(context, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
        };
        r = vkAllocateMemory(context->device(), &allocInfo, nullptr, &memory);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to allocate texture image memory");
        }

        vkBindImageMemory(context->device(), image, memory, 0);

        VkImageViewCreateInfo createInfo = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = image,
            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = format,
            .components = {
                .r = VK_COMPONENT_SWIZZLE_IDENTITY,
                .g = VK_COMPONENT_SWIZZLE_IDENTITY,
                .b = VK_COMPONENT_SWIZZLE_IDENTITY,
                .a = VK_COMPONENT_SWIZZLE_IDENTITY,
            },
            .subresourceRange = {
                .aspectMask = imageAspect,
                .baseMipLevel = 0,
                .levelCount = 1,
                .baseArrayLayer = 0,
                .layerCount = 1
            }
        };
        r = vkCreateImageView(context->device(), &createInfo, nullptr, &imageView);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("failed to create image view");
        }
    }

    void destroy(const VulkanContext *context)
    {
        if (image != VK_NULL_HANDLE)
        {
            vkDestroyImage(context->device(), image, nullptr);
            image = VK_NULL_HANDLE;
        }

        if (imageView != VK_NULL_HANDLE)
        {
            vkDestroyImageView(context->device(), imageView, nullptr);
            imageView = VK_NULL_HANDLE;
        }

        if (memory != VK_NULL_HANDLE)
        {
            vkFreeMemory(context->device(), memory, nullptr);
            memory = VK_NULL_HANDLE;
        }
    }

    bool allocated() const
    {
        return image != VK_NULL_HANDLE && imageView != VK_NULL_HANDLE && memory != VK_NULL_HANDLE;
    }

    template <typename T>
    void copyFrom(VkCommandBuffer commandBuffer, const VulkanMappedBuffer<T>& staging)
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
            staging.buffer,
            image,
            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            1,
            &region
        );
    }

private:
    static uint32_t findMemoryType(const VulkanContext *context, uint32_t typeFilter, VkMemoryPropertyFlags properties)
    {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(context->physicalDevice(), &memProperties);

        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
        {
            if (typeFilter & 1 << i && (memProperties.memoryTypes[i].propertyFlags & properties) == properties)
            {
                return i;
            }
        }

        throw std::runtime_error("Failed to find suitable memory type");
    }
};


#endif //BUFFER_STRUCT_VULKANBUFFER_H
