#ifndef BUFFER_STRUCT_VULKANBUFFER_H
#define BUFFER_STRUCT_VULKANBUFFER_H

#include <stdexcept>

#include <vulkan/vulkan.h>
#include <vk_mem_alloc.h>

#include "VulkanContext.h"

template <typename T>
struct VulkanMappedBuffer
{
    VkBuffer buffer = VK_NULL_HANDLE;
    VmaAllocation allocation = VK_NULL_HANDLE;
    VkDeviceSize size = 0;
    T *mappedData = nullptr;

    VulkanMappedBuffer() = default;
    VulkanMappedBuffer(const VulkanMappedBuffer&) = delete;
    VulkanMappedBuffer& operator=(const VulkanMappedBuffer&) = delete;
    VulkanMappedBuffer& operator=(VulkanMappedBuffer&&) = delete;

    VulkanMappedBuffer(VulkanMappedBuffer&& other) noexcept
        : buffer(other.buffer), allocation(other.allocation), size(other.size), mappedData(other.mappedData)
    {
        other.buffer = VK_NULL_HANDLE;
        other.allocation = VK_NULL_HANDLE;
        other.mappedData = nullptr;
        other.size = 0;
    }

    void create(
        const VulkanContext *context,
        VkDeviceSize bufSize,
        VkBufferUsageFlags usage
    )
    {
        size = bufSize;

        VkBufferCreateInfo bufferInfo = {
            .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
            .size = size,
            .usage = usage,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE
        };
        VmaAllocationCreateInfo allocInfo = {
            .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT | VMA_ALLOCATION_CREATE_MAPPED_BIT,
            .usage = VMA_MEMORY_USAGE_AUTO_PREFER_HOST,
        };
        VmaAllocationInfo allocationInfo;
        VkResult r = vmaCreateBuffer(context->allocator(), &bufferInfo, &allocInfo, &buffer, &allocation, &allocationInfo);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("failed to create buffer");
        }

        mappedData = static_cast<T*>(allocationInfo.pMappedData);
    }

    void destroy(const VulkanContext *context)
    {
        if (buffer != VK_NULL_HANDLE)
        {
            vmaDestroyBuffer(context->allocator(), buffer, allocation);
            buffer = VK_NULL_HANDLE;
            allocation = VK_NULL_HANDLE;
        }
    }

    bool allocated() const
    {
        return buffer != VK_NULL_HANDLE && allocation != VK_NULL_HANDLE;
    }
};

struct VulkanBuffer
{
    VkBuffer buffer = VK_NULL_HANDLE;
    VmaAllocation allocation = VK_NULL_HANDLE;

    VulkanBuffer() = default;
    VulkanBuffer(const VulkanBuffer&) = delete;
    VulkanBuffer& operator=(const VulkanBuffer&) = delete;

    VulkanBuffer(VulkanBuffer&& other) noexcept
        : buffer(other.buffer), allocation(other.allocation)
    {
        other.buffer = VK_NULL_HANDLE;
        other.allocation = VK_NULL_HANDLE;
    }

    VulkanBuffer& operator=(VulkanBuffer&&) = delete;

    void create(
        const VulkanContext *context,
        VkDeviceSize size,
        VkBufferUsageFlags usage
    )
    {
        VkBufferCreateInfo bufferInfo = {
            .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
            .size = size,
            .usage = usage,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE
        };
        VmaAllocationCreateInfo allocInfo = {
            .usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE
        };
        VkResult r = vmaCreateBuffer(context->allocator(), &bufferInfo, &allocInfo, &buffer, &allocation, nullptr);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create buffer");
        }
    }

    void destroy(const VulkanContext *context)
    {
        if (buffer != VK_NULL_HANDLE)
        {
            vmaDestroyBuffer(context->allocator(), buffer, allocation);
            buffer = VK_NULL_HANDLE;
            allocation = VK_NULL_HANDLE;
        }
    }

    bool allocated() const
    {
        return buffer != VK_NULL_HANDLE && allocation != VK_NULL_HANDLE;
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
};

struct VulkanImage
{
    VkImage image = VK_NULL_HANDLE;
    VkImageView imageView = VK_NULL_HANDLE;
    VmaAllocation allocation = VK_NULL_HANDLE;
    uint32_t width = 0;
    uint32_t height = 0;

    VulkanImage() = default;
    VulkanImage(const VulkanImage&) = delete;
    VulkanImage& operator=(const VulkanImage&) = delete;
    VulkanImage& operator=(VulkanImage&&) = delete;

    VulkanImage(VulkanImage&& other) noexcept
        : image(other.image), imageView(other.imageView), allocation(other.allocation), width(other.width), height(other.height)
    {
        other.image = VK_NULL_HANDLE;
        other.imageView = VK_NULL_HANDLE;
        other.allocation = nullptr;
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
        VmaAllocationCreateInfo allocInfo = {
            .usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE
        };
        VkResult r = vmaCreateImage(context->allocator(), &imageInfo, &allocInfo, &image, &allocation, nullptr);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create texture image");
        }

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
            vmaDestroyImage(context->allocator(), image, allocation);
            image = VK_NULL_HANDLE;
            allocation = VK_NULL_HANDLE;
        }

        if (imageView != VK_NULL_HANDLE)
        {
            vkDestroyImageView(context->device(), imageView, nullptr);
            imageView = VK_NULL_HANDLE;
        }
    }

    bool allocated() const
    {
        return image != VK_NULL_HANDLE && imageView != VK_NULL_HANDLE && allocation != VK_NULL_HANDLE;
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
};


#endif //BUFFER_STRUCT_VULKANBUFFER_H
