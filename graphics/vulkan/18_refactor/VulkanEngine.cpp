#include "VulkanEngine.h"

#include <cstdint>

#include <algorithm>
#include <stdexcept>
#include <ranges>
#include <format>
#include <limits>
#include <fstream>
#include <chrono>

#include <vulkan/vulkan_win32.h>
#include <glm/gtc/matrix_transform.hpp>

VulkanEngine::VulkanEngine(GLFWwindow* window, VulkanContext *context)
    : m_window(window),
    m_context(context)
{
    createSurface();
    createSwapchain();
    createImageViews();
    createDescriptorSetLayout();
    createPipeline();

    createVertexBuffer();
    createIndexBuffer();
    createUniformBuffers();
    createDescriptorSets();

    createCommandBuffer();
    createSyncObjects();
}

VulkanEngine::~VulkanEngine()
{
    vkDeviceWaitIdle(m_context->device());

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        vkDestroyBuffer(m_context->device(), m_uniformBuffers[i], nullptr);
        vkFreeMemory(m_context->device(), m_uniformBuffersMemory[i], nullptr);
    }
    vkDestroyBuffer(m_context->device(), m_vertexBuffer, nullptr);
    vkFreeMemory(m_context->device(), m_vertexBufferMemory, nullptr);
    vkDestroyBuffer(m_context->device(), m_indexBuffer, nullptr);
    vkFreeMemory(m_context->device(), m_indexBufferMemory, nullptr);

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        vkDestroySemaphore(m_context->device(), m_imageAvailableSemaphores[i], nullptr);
        vkDestroyFence(m_context->device(), m_drawFences[i], nullptr);
    }
    for (size_t i = 0; i < m_swapchainImages.size(); i++)
    {
        vkDestroySemaphore(m_context->device(), m_renderCompleteSemaphores[i], nullptr);
    }

    vkDestroyDescriptorSetLayout(m_context->device(), m_descriptorSetLayout, nullptr);

    vkDestroyPipeline(m_context->device(), m_graphicsPipeline, nullptr);
    vkDestroyPipelineLayout(m_context->device(), m_pipelineLayout, nullptr);

    cleanupSwapchain();
    vkDestroySurfaceKHR(m_context->instance(), m_surface, nullptr);
}

void VulkanEngine::render()
{
    VkResult r = vkWaitForFences(m_context->device(), 1, &m_drawFences[m_currentFrame], VK_TRUE, std::numeric_limits<uint64_t>::max());
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to wait for draw fence");
    }

    uint32_t imageIndex;
    r = vkAcquireNextImageKHR(
        m_context->device(),
        m_swapchain,
        std::numeric_limits<uint64_t>::max(),
        m_imageAvailableSemaphores[m_currentFrame], // signal
        VK_NULL_HANDLE,
        &imageIndex
    );
    if (r == VK_ERROR_OUT_OF_DATE_KHR || r == VK_SUBOPTIMAL_KHR || isResized)
    {
        isResized = false;
        recreateSwapchain();
        return;
    }
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to acquire next image from swapchain");
    }

    updateUniformBuffer(m_currentFrame);

    r = vkResetFences(m_context->device(), 1, &m_drawFences[m_currentFrame]);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to reset draw fence");
    }

    vkResetCommandBuffer(m_commandBuffers[m_currentFrame], 0);
    recordCommandBuffer(imageIndex);

    VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
    VkSubmitInfo submitInfo = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &m_imageAvailableSemaphores[m_currentFrame],
        .pWaitDstStageMask = waitStages,
        .commandBufferCount = 1,
        .pCommandBuffers = &m_commandBuffers[m_currentFrame],
        .signalSemaphoreCount = 1,
        .pSignalSemaphores = &m_renderCompleteSemaphores[imageIndex]
    };
    r = vkQueueSubmit(m_context->graphicsQueue(), 1, &submitInfo, m_drawFences[m_currentFrame]);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to submit draw command buffer");
    }

    VkPresentInfoKHR presentInfo = {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &m_renderCompleteSemaphores[imageIndex],
        .swapchainCount = 1,
        .pSwapchains = &m_swapchain,
        .pImageIndices = &imageIndex
    };
    r = vkQueuePresentKHR(m_context->graphicsQueue(), &presentInfo);
    if (r == VK_ERROR_OUT_OF_DATE_KHR || r == VK_SUBOPTIMAL_KHR)
    {
        recreateSwapchain();
        return;
    }
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to present swapchain image");
    }

    m_currentFrame = (m_currentFrame + 1) % MAX_FRAMES_IN_FLIGHT;
}

void VulkanEngine::createSurface()
{
    VkWin32SurfaceCreateInfoKHR createInfo = {
        .sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
        .hinstance = GetModuleHandle(nullptr),
        .hwnd = glfwGetWin32Window(m_window)
    };

    VkResult r = vkCreateWin32SurfaceKHR(m_context->instance(), &createInfo, nullptr, &m_surface);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create window surface");
    }
}

void VulkanEngine::createSwapchain()
{
    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(m_context->physicalDevice(), m_surface, &surfaceCapabilities);

    VkSurfaceFormatKHR surfaceFormat = chooseSwapSurfaceFormat();
    VkPresentModeKHR presentMode = chooseSwapPresentMode();
    VkExtent2D extent = chooseSwapExtent(m_window);

    uint32_t imageCount = surfaceCapabilities.minImageCount + 1;
    if (surfaceCapabilities.maxImageCount > 0 && imageCount > surfaceCapabilities.maxImageCount)
    {
        imageCount = surfaceCapabilities.maxImageCount;
    }

    VkSwapchainCreateInfoKHR createInfo = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = m_surface,
        .minImageCount = imageCount,
        .imageFormat = surfaceFormat.format,
        .imageColorSpace = surfaceFormat.colorSpace,
        .imageExtent = extent,
        .imageArrayLayers = 1,
        .imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        .imageSharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .preTransform = surfaceCapabilities.currentTransform,
        .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = presentMode,
        .clipped = VK_TRUE
    };
    VkResult r = vkCreateSwapchainKHR(m_context->device(), &createInfo, nullptr, &m_swapchain);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create swapchain");
    }

    vkGetSwapchainImagesKHR(m_context->device(), m_swapchain, &imageCount, nullptr);
    m_swapchainImages.resize(imageCount);
    vkGetSwapchainImagesKHR(m_context->device(), m_swapchain, &imageCount, m_swapchainImages.data());

    m_swapchainImageFormat = surfaceFormat;
    m_swapchainExtent = extent;
}

VkSurfaceFormatKHR VulkanEngine::chooseSwapSurfaceFormat() const
{
    uint32_t availableFormatCount = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(m_context->physicalDevice(), m_surface, &availableFormatCount, nullptr);
    std::vector<VkSurfaceFormatKHR> availableFormats(availableFormatCount);
    vkGetPhysicalDeviceSurfaceFormatsKHR(m_context->physicalDevice(), m_surface, &availableFormatCount, availableFormats.data());

    auto it = std::ranges::find_if(availableFormats, [](const VkSurfaceFormatKHR& availableFormat)
    {
        return availableFormat.format == VK_FORMAT_B8G8R8A8_SRGB && availableFormat.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
    });

    return it != availableFormats.end() ? *it : availableFormats[0];
}

VkPresentModeKHR VulkanEngine::chooseSwapPresentMode() const
{
    uint32_t availablePresentModeCount = 0;
    vkGetPhysicalDeviceSurfacePresentModesKHR(m_context->physicalDevice(), m_surface, &availablePresentModeCount, nullptr);
    std::vector<VkPresentModeKHR> availablePresentModes(availablePresentModeCount);
    vkGetPhysicalDeviceSurfacePresentModesKHR(m_context->physicalDevice(), m_surface, &availablePresentModeCount, availablePresentModes.data());

    auto it = std::ranges::find(availablePresentModes, VK_PRESENT_MODE_MAILBOX_KHR);
    return it != availablePresentModes.end() ? *it : VK_PRESENT_MODE_FIFO_KHR;
}

VkExtent2D VulkanEngine::chooseSwapExtent(GLFWwindow* window) const
{
    VkSurfaceCapabilitiesKHR  surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(m_context->physicalDevice(), m_surface, &surfaceCapabilities);

    if (surfaceCapabilities.currentExtent.width != std::numeric_limits<uint32_t>::max())
    {
        return surfaceCapabilities.currentExtent;
    }

    int width, height;
    glfwGetFramebufferSize(window, &width, &height);

    VkExtent2D actualExtent = {
        .width = std::clamp(static_cast<uint32_t>(width), surfaceCapabilities.minImageExtent.width, surfaceCapabilities.maxImageExtent.width),
        .height = std::clamp(static_cast<uint32_t>(height), surfaceCapabilities.minImageExtent.height, surfaceCapabilities.maxImageExtent.height)
    };
    return actualExtent;
}

void VulkanEngine::createImageViews()
{
    VkImageViewCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .viewType = VK_IMAGE_VIEW_TYPE_2D,
        .format = m_swapchainImageFormat.format,
        .components = {
            .r = VK_COMPONENT_SWIZZLE_IDENTITY,
            .g = VK_COMPONENT_SWIZZLE_IDENTITY,
            .b = VK_COMPONENT_SWIZZLE_IDENTITY,
            .a = VK_COMPONENT_SWIZZLE_IDENTITY
        },
        .subresourceRange = {
            .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .baseMipLevel = 0,
            .levelCount = 1,
            .baseArrayLayer = 0,
            .layerCount = 1
        }
    };

    m_swapchainImageViews.reserve(m_swapchainImages.size());
    for (const auto& image : m_swapchainImages)
    {
        createInfo.image = image;

        VkImageView imageView;
        VkResult r = vkCreateImageView(m_context->device(), &createInfo, nullptr, &imageView);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create image view");
        }

        m_swapchainImageViews.push_back(imageView);
    }
}

void VulkanEngine::createPipeline()
{
    VkShaderModule module = createShaderModule("shaders/shaders.spv");

    VkPipelineShaderStageCreateInfo vertShaderStageInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_VERTEX_BIT,
        .module = module,
        .pName = "vertMain"
    };
    VkPipelineShaderStageCreateInfo fragShaderStageInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
        .module = module,
        .pName = "fragMain"
    };

    std::array shaderStages = {vertShaderStageInfo, fragShaderStageInfo};

    std::array dynamicStates = {
        VK_DYNAMIC_STATE_VIEWPORT,
        VK_DYNAMIC_STATE_SCISSOR
    };
    VkPipelineDynamicStateCreateInfo dynamicStateCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
        .dynamicStateCount = static_cast<uint32_t>(dynamicStates.size()),
        .pDynamicStates = dynamicStates.data()
    };

    VkVertexInputBindingDescription bindingDescription = Vertex::getBindingDescription();
    std::array<VkVertexInputAttributeDescription, 2> attributeDescriptions = Vertex::getAttributeDescriptions();
    VkPipelineVertexInputStateCreateInfo vertexInputInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        .vertexBindingDescriptionCount = 1,
        .pVertexBindingDescriptions = &bindingDescription,
        .vertexAttributeDescriptionCount = static_cast<uint32_t>(attributeDescriptions.size()),
        .pVertexAttributeDescriptions = attributeDescriptions.data()
    };

    VkPipelineInputAssemblyStateCreateInfo inputAssembly = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        .topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        .primitiveRestartEnable = VK_FALSE
    };

    m_viewport = {
        .x = 0.0f,
        .y = 0.0f,
        .width = static_cast<float>(m_swapchainExtent.width),
        .height = static_cast<float>(m_swapchainExtent.height),
        .minDepth = 0.0f,
        .maxDepth = 1.0f
    };
    m_scissor = {
        .offset = {0, 0},
        .extent = m_swapchainExtent
    };

    VkPipelineViewportStateCreateInfo viewportState = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
        .viewportCount = 1,
        .scissorCount = 1,
    };

    VkPipelineRasterizationStateCreateInfo rasterizer = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        .depthClampEnable = VK_FALSE,
        .rasterizerDiscardEnable = VK_FALSE,
        .polygonMode = VK_POLYGON_MODE_FILL,
        .cullMode = VK_CULL_MODE_NONE,
        .frontFace = VK_FRONT_FACE_CLOCKWISE,
        .depthBiasEnable = VK_FALSE,
        .lineWidth = 1.0f
    };

    VkPipelineMultisampleStateCreateInfo multisampling = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
        .rasterizationSamples = VK_SAMPLE_COUNT_1_BIT,
        .sampleShadingEnable = VK_FALSE
    };

    VkPipelineColorBlendAttachmentState colorBlendAttachment = {
        .blendEnable = VK_FALSE,
        .colorWriteMask = VK_COLOR_COMPONENT_R_BIT
                        | VK_COLOR_COMPONENT_G_BIT
                        | VK_COLOR_COMPONENT_B_BIT
                        | VK_COLOR_COMPONENT_A_BIT
    };
    VkPipelineColorBlendStateCreateInfo colorBlending = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
        .logicOpEnable = VK_FALSE,
        .attachmentCount = 1,
        .pAttachments = &colorBlendAttachment
    };

    VkPipelineLayoutCreateInfo pipelineLayoutInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .setLayoutCount = 1,
        .pSetLayouts = &m_descriptorSetLayout,
        .pushConstantRangeCount = 0
    };
    VkResult r = vkCreatePipelineLayout(m_context->device(), &pipelineLayoutInfo, nullptr, &m_pipelineLayout);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create pipeline layout");
    }

    VkPipelineRenderingCreateInfo renderingCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO,
        .colorAttachmentCount = 1,
        .pColorAttachmentFormats = &m_swapchainImageFormat.format
    };

    VkGraphicsPipelineCreateInfo pipelineInfo = {
        .sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
        .pNext = &renderingCreateInfo,
        .stageCount = static_cast<uint32_t>(shaderStages.size()),
        .pStages = shaderStages.data(),
        .pVertexInputState = &vertexInputInfo,
        .pInputAssemblyState = &inputAssembly,
        .pViewportState = &viewportState,
        .pRasterizationState = &rasterizer,
        .pMultisampleState = &multisampling,
        .pColorBlendState = &colorBlending,
        .pDynamicState = &dynamicStateCreateInfo,
        .layout = m_pipelineLayout,
        .renderPass = VK_NULL_HANDLE, // Using dynamic rendering, so no render pass is used here
        .subpass = 0
    };

    r = vkCreateGraphicsPipelines(m_context->device(), VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &m_graphicsPipeline);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create graphics pipeline");
    }

    vkDestroyShaderModule(m_context->device(), module, nullptr);
}

VkShaderModule VulkanEngine::createShaderModule(const std::string& filePath) const
{
    std::ifstream file(filePath, std::ios::ate | std::ios::binary);
    if (!file.is_open())
    {
        throw std::runtime_error("Failed to open shader file: " + filePath);
    }

    std::vector<std::byte> code(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(reinterpret_cast<char*>(code.data()), code.size());
    file.close();

    VkShaderModuleCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize = code.size(),
        .pCode = reinterpret_cast<const uint32_t*>(code.data())
    };
    VkShaderModule shaderModule;
    VkResult r = vkCreateShaderModule(m_context->device(), &createInfo, nullptr, &shaderModule);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create shader module for file: " + filePath);
    }

    return shaderModule;
}

void VulkanEngine::createCommandBuffer()
{
    VkCommandBufferAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = m_context->commandPool(),
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = MAX_FRAMES_IN_FLIGHT
    };
    m_commandBuffers.resize(MAX_FRAMES_IN_FLIGHT);
    VkResult r = vkAllocateCommandBuffers(m_context->device(), &allocInfo, m_commandBuffers.data());
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to allocate command buffers");
    }
}

void VulkanEngine::recordCommandBuffer(uint32_t imageIndex) const
{
    VkCommandBuffer commandBuffer = m_commandBuffers[m_currentFrame];

    VkCommandBufferBeginInfo beginInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
    };
    VkResult r = vkBeginCommandBuffer(commandBuffer, &beginInfo);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to begin recording command buffer");
    }

    transitionImageLayout(
        imageIndex,
        VK_IMAGE_LAYOUT_UNDEFINED,
        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        0,
        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    );

    VkClearValue clearColor = {{{0.0f, 0.0f, 0.0f, 1.0f}}};
    VkRenderingAttachmentInfo colorAttachment = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
        .imageView = m_swapchainImageViews[imageIndex],
        .imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,
        .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
        .clearValue = clearColor
    };

    VkRenderingInfo renderingInfo = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_INFO,
        .renderArea = {
            .offset = {0, 0},
            .extent = m_swapchainExtent
        },
        .layerCount = 1,
        .viewMask = 0,
        .colorAttachmentCount = 1,
        .pColorAttachments = &colorAttachment
    };

    vkCmdBeginRendering(commandBuffer, &renderingInfo);

    vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, m_graphicsPipeline);

    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, &m_vertexBuffer, offsets);
    vkCmdBindIndexBuffer(commandBuffer, m_indexBuffer, 0, VK_INDEX_TYPE_UINT16);
    vkCmdBindDescriptorSets(
        commandBuffer,
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        m_pipelineLayout,
        0,
        1,
        &m_descriptorSets[m_currentFrame],
        0,
        nullptr
    );

    vkCmdSetViewport(commandBuffer, 0, 1, &m_viewport);
    vkCmdSetScissor(commandBuffer, 0, 1, &m_scissor);

    vkCmdDrawIndexed(commandBuffer, static_cast<uint32_t>(indices.size()), 1, 0, 0, 0);

    vkCmdEndRendering(commandBuffer);

    transitionImageLayout(
        imageIndex,
        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        0,
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    );

    r = vkEndCommandBuffer(commandBuffer);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to record command buffer");
    }
}

void VulkanEngine::transitionImageLayout(
    uint32_t imageIndex,
    VkImageLayout oldLayout,
    VkImageLayout newLayout,
    VkAccessFlags2 srcAccessMask,
    VkAccessFlags2 dstAccessMask,
    VkPipelineStageFlags2 srcStageMask,
    VkPipelineStageFlags2 dstStageMask
) const
{
    VkImageMemoryBarrier2 barrier = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
        .srcStageMask = srcStageMask,
        .srcAccessMask = srcAccessMask,
        .dstStageMask = dstStageMask,
        .dstAccessMask = dstAccessMask,
        .oldLayout = oldLayout,
        .newLayout = newLayout,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .image = m_swapchainImages[imageIndex],
        .subresourceRange = {
            .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .baseMipLevel = 0,
            .levelCount = 1,
            .baseArrayLayer = 0,
            .layerCount = 1
        }
    };

    VkDependencyInfo dependencyInfo = {
        .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .imageMemoryBarrierCount = 1,
        .pImageMemoryBarriers = &barrier
    };
    vkCmdPipelineBarrier2(m_commandBuffers[m_currentFrame], &dependencyInfo);
}

void VulkanEngine::createSyncObjects()
{
    VkSemaphoreCreateInfo semaphoreInfo = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
    };
    VkFenceCreateInfo fenceInfo = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT
    };
    
    m_imageAvailableSemaphores.resize(MAX_FRAMES_IN_FLIGHT);
    m_renderCompleteSemaphores.resize(m_swapchainImages.size());
    m_drawFences.resize(MAX_FRAMES_IN_FLIGHT);
    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        VkResult r = vkCreateSemaphore(m_context->device(), &semaphoreInfo, nullptr, &m_imageAvailableSemaphores[i]);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create image available semaphore");
        }
        
        r = vkCreateFence(m_context->device(), &fenceInfo, nullptr, &m_drawFences[i]);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create draw fence");
        }
    }
    for (size_t i = 0; i < m_swapchainImages.size(); i++)
    {
        VkResult r = vkCreateSemaphore(m_context->device(), &semaphoreInfo, nullptr, &m_renderCompleteSemaphores[i]);
        if (r != VK_SUCCESS)
        {
            throw std::runtime_error("Failed to create render complete semaphore");
        }
    }
}

void VulkanEngine::createDescriptorSetLayout()
{
    VkDescriptorSetLayoutBinding uboLayoutBinding = {
        .binding = 0,
        .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        .descriptorCount = 1,
        .stageFlags = VK_SHADER_STAGE_VERTEX_BIT
    };
    VkDescriptorSetLayoutCreateInfo layoutInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .bindingCount = 1,
        .pBindings = &uboLayoutBinding
    };
    VkResult r = vkCreateDescriptorSetLayout(m_context->device(), &layoutInfo, nullptr, &m_descriptorSetLayout);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create descriptor set layout");
    }
}

void VulkanEngine::createDescriptorSets()
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
        VkWriteDescriptorSet descriptorWrite = {
            .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
            .dstSet = m_descriptorSets[i],
            .dstBinding = 0,
            .dstArrayElement = 0,
            .descriptorCount = 1,
            .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            .pBufferInfo = &bufferInfo
        };
        vkUpdateDescriptorSets(m_context->device(), 1, &descriptorWrite, 0, nullptr);
    }
}

void VulkanEngine::recreateSwapchain()
{
    int width, height;
    glfwGetFramebufferSize(m_window, &width, &height);
    while (width == 0 || height == 0)
    {
        glfwGetFramebufferSize(m_window, &width, &height);
        glfwWaitEvents();
    }

    vkDeviceWaitIdle(m_context->device());

    cleanupSwapchain();
    createSwapchain();
    createImageViews();
}

void VulkanEngine::cleanupSwapchain()
{
    for (const auto& imageView : m_swapchainImageViews)
    {
        vkDestroyImageView(m_context->device(), imageView, nullptr);
    }
    m_swapchainImageViews.clear();

    vkDestroySwapchainKHR(m_context->device(), m_swapchain, nullptr);
    m_swapchain = VK_NULL_HANDLE;
}

void VulkanEngine::updateUniformBuffer(uint32_t currentImage) const
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
        static_cast<float>(m_swapchainExtent.width) / static_cast<float>(m_swapchainExtent.height),
        0.1f,
        10.0f
    );
    ubo.proj[1][1] *= -1; // Invert Y coordinate for Vulkan

    memcpy(m_uniformBuffersMapped[currentImage], &ubo, sizeof(ubo));
}

void VulkanEngine::createVertexBuffer()
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

void VulkanEngine::createIndexBuffer()
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

void VulkanEngine::createUniformBuffers()
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

uint32_t VulkanEngine::findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties) const
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

std::pair<VkBuffer, VkDeviceMemory> VulkanEngine::createBuffer(
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

void VulkanEngine::copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size) const
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
