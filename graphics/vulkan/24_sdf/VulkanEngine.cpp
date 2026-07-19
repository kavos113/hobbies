#include "VulkanEngine.h"

#include <cstdint>

#include <algorithm>
#include <stdexcept>
#include <ranges>
#include <format>
#include <limits>
#include <fstream>

#include <vulkan/vulkan_win32.h>

#include "VulkanHelper.h"

VulkanEngine::VulkanEngine(GLFWwindow* window, VulkanContext *context)
    : m_window(window),
    m_context(context)
{
    createSurface();
    createSwapchain();
    createImageViews();

    m_object = std::make_unique<Object>(m_context);

    createPipeline();

    createCommandBuffer();
    createSyncObjects();
}

VulkanEngine::~VulkanEngine()
{
    vkDeviceWaitIdle(m_context->device());

    m_object.reset();

    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        vkDestroySemaphore(m_context->device(), m_imageAvailableSemaphores[i], nullptr);
        vkDestroyFence(m_context->device(), m_drawFences[i], nullptr);
    }
    for (size_t i = 0; i < m_swapchainImages.size(); i++)
    {
        vkDestroySemaphore(m_context->device(), m_renderCompleteSemaphores[i], nullptr);
    }

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

    m_object->beforeRender(m_currentFrame, static_cast<float>(m_swapchainExtent.width), static_cast<float>(m_swapchainExtent.height));

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

    VkVertexInputBindingDescription bindingDescription = Object::Vertex::getBindingDescription();
    std::array<VkVertexInputAttributeDescription, 2> attributeDescriptions = Object::Vertex::getAttributeDescriptions();
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
        .setLayoutCount = 0,
        .pSetLayouts = nullptr,
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
        commandBuffer,
        m_swapchainImages[imageIndex],
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

    vkCmdSetViewport(commandBuffer, 0, 1, &m_viewport);
    vkCmdSetScissor(commandBuffer, 0, 1, &m_scissor);

    m_object->render(commandBuffer, m_pipelineLayout, m_currentFrame);

    vkCmdEndRendering(commandBuffer);

    transitionImageLayout(
        commandBuffer,
        m_swapchainImages[imageIndex],
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
