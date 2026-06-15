#include "VulkanEngine.h"

#include <cstdint>

#include <array>
#include <algorithm>
#include <stdexcept>
#include <map>
#include <ranges>
#include <format>
#include <limits>
#include <fstream>

#include <vulkan/vulkan_win32.h>

namespace
{
std::vector<std::string> requiredDeviceExtensions = {
    VK_KHR_SWAPCHAIN_EXTENSION_NAME
};

int findQueueFamilies(VkPhysicalDevice device)
{
    uint32_t queueFamilyCount = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, nullptr);
    std::vector<VkQueueFamilyProperties> queueFamilies(queueFamilyCount);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, queueFamilies.data());

    auto graphicsQueueFamilyIt = std::ranges::find_if(queueFamilies, [](const VkQueueFamilyProperties& queueFamily)
    {
        return queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT;
    });

    if (graphicsQueueFamilyIt == queueFamilies.end())
    {
        return -1;
    }
    else
    {
        return static_cast<int>(std::distance(queueFamilies.begin(), graphicsQueueFamilyIt));
    }
}

int rateDeviceSuitability(VkPhysicalDevice device)
{
    VkPhysicalDeviceProperties  deviceProperties;
    VkPhysicalDeviceFeatures deviceFeatures;
    vkGetPhysicalDeviceProperties(device, &deviceProperties);
    vkGetPhysicalDeviceFeatures(device, &deviceFeatures);

    int score = 0;

    if (!deviceFeatures.geometryShader)
    {
        return 0;
    }

    if (findQueueFamilies(device) == -1)
    {
        return 0;
    }

    if (deviceProperties.apiVersion < VK_API_VERSION_1_3)
    {
        score -= 10000;
    }

    if (deviceProperties.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
    {
        score += 1000;
    }

    if (deviceProperties.deviceType == VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
    {
        score += 100;
    }

    return score;
}
}

VulkanEngine::VulkanEngine(GLFWwindow* window)
{
    createInstance();

    if (m_enableValidationLayers)
    {
        m_debug = std::make_unique<VulkanDebug>(m_instance);
    }

    pickPhysicalDevice();
    createLogicalDevice();
    createSurface(window);
    createSwapchain(window);
    createImageViews();
    createPipeline();
    createCommandPool();
    createCommandBuffer();
}

VulkanEngine::~VulkanEngine()
{
    vkDestroyPipeline(m_device, m_graphicsPipeline, nullptr);
    vkDestroyPipelineLayout(m_device, m_pipelineLayout, nullptr);

    for (const auto& imageView : m_swapchainImageViews)
    {
        vkDestroyImageView(m_device, imageView, nullptr);
    }

    vkDestroySwapchainKHR(m_device, m_swapchain, nullptr);
    vkDestroySurfaceKHR(m_instance, m_surface, nullptr);
    vkDestroyDevice(m_device, nullptr);

    m_debug->cleanup(m_instance);
    m_debug.reset();

    vkDestroyInstance(m_instance, nullptr);
}

void VulkanEngine::render()
{
}

void VulkanEngine::createInstance()
{
    VkApplicationInfo appInfo = {
        .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = "Hello Vulkan",
        .applicationVersion = VK_MAKE_VERSION(1, 0, 0),
        .pEngineName = "No Engine",
        .engineVersion = VK_MAKE_VERSION(1, 0, 0),
        .apiVersion = VK_API_VERSION_1_0
    };

    uint32_t glfwExtensionCount = 0;
    const char** glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);

    uint32_t extensionCount = 0;
    vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, nullptr);
    std::vector<VkExtensionProperties> availableExtensions(extensionCount);
    vkEnumerateInstanceExtensionProperties(nullptr, &extensionCount, availableExtensions.data());

    for (uint32_t i = 0; i < glfwExtensionCount; i++)
    {
        bool notFound = std::ranges::none_of(availableExtensions, [glfwExtension = glfwExtensions[i]](const VkExtensionProperties& extension)
        {
            return strcmp(glfwExtension, extension.extensionName) == 0;
        });
        if (notFound)
        {
            throw std::runtime_error("Required Vulkan extension not found: " + std::string(glfwExtensions[i]));
        }
    }

    std::vector<std::string> enabledLayerNames;
    std::vector<std::string> enabledExtensionNames(glfwExtensions, glfwExtensions + glfwExtensionCount);

    if (m_enableValidationLayers)
    {
        VulkanDebug::addDebugSettings(enabledExtensionNames, enabledLayerNames);
    }

    std::vector<const char*> enabledLayerNamePtrs = enabledLayerNames
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();
    std::vector<const char*> enabledExtensionNamePtrs = enabledExtensionNames
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();

    VkInstanceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &appInfo,
        .enabledLayerCount = static_cast<uint32_t>(enabledLayerNamePtrs.size()),
        .ppEnabledLayerNames = enabledLayerNamePtrs.data(),
        .enabledExtensionCount = static_cast<uint32_t>(enabledExtensionNamePtrs.size()),
        .ppEnabledExtensionNames = enabledExtensionNamePtrs.data()
    };

    VkResult r = vkCreateInstance(&createInfo, nullptr, &m_instance);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create Vulkan instance");
    }
}

void VulkanEngine::pickPhysicalDevice()
{
    uint32_t deviceCount = 0;
    vkEnumeratePhysicalDevices(m_instance, &deviceCount, nullptr);
    if (deviceCount == 0)
    {
        throw std::runtime_error("Failed to find any Vulkan-compatible GPU");
    }

    std::vector<VkPhysicalDevice> devices(deviceCount);
    vkEnumeratePhysicalDevices(m_instance, &deviceCount, devices.data());

    std::multimap<int, VkPhysicalDevice> candidates;
    for (const auto& device : devices)
    {
        int score = rateDeviceSuitability(device);
        candidates.insert({score, device});
    }

    if (candidates.rbegin()->first > 0)
    {
        m_physicalDevice = candidates.rbegin()->second;
    }
    else
    {
        throw std::runtime_error("Failed to find a suitable GPU");
    }
}

void VulkanEngine::createLogicalDevice()
{
    int graphicsQueueFamilyIndex = findQueueFamilies(m_physicalDevice);
    if (graphicsQueueFamilyIndex == -1)
    {
        throw std::runtime_error("Failed to find a queue family that supports graphics commands");
    }

    float queuePriority = 1.0f;
    VkDeviceQueueCreateInfo queueCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        .queueFamilyIndex = static_cast<uint32_t>(graphicsQueueFamilyIndex),
        .queueCount = 1,
        .pQueuePriorities = &queuePriority
    };

    VkPhysicalDeviceExtendedDynamicStateFeaturesEXT extendedDynamicStateFeatures = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT,
        .extendedDynamicState = VK_TRUE
    };
    VkPhysicalDeviceVulkan13Features deviceFeatures = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
        .pNext = &extendedDynamicStateFeatures,
        .dynamicRendering = VK_TRUE
    };
    VkPhysicalDeviceVulkan11Features device11Features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
        .pNext = &deviceFeatures,
        .shaderDrawParameters = VK_TRUE
    };
    VkPhysicalDeviceFeatures2 deviceFeatures2 = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
        .pNext = &device11Features
    };

    std::vector<const char*> enabledExtensionNamePtrs = requiredDeviceExtensions
        | std::views::transform([](const std::string& name) { return name.c_str(); })
        | std::ranges::to<std::vector>();

    VkDeviceCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &deviceFeatures2,
        .queueCreateInfoCount = 1,
        .pQueueCreateInfos = &queueCreateInfo,
        .enabledExtensionCount = static_cast<uint32_t>(enabledExtensionNamePtrs.size()),
        .ppEnabledExtensionNames = enabledExtensionNamePtrs.data()
    };
    VkResult r = vkCreateDevice(m_physicalDevice, &createInfo, nullptr, &m_device);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create logical device");
    }

    vkGetDeviceQueue(m_device, graphicsQueueFamilyIndex, 0, &m_graphicsQueue);
}

void VulkanEngine::createSurface(GLFWwindow* window)
{
    VkWin32SurfaceCreateInfoKHR createInfo = {
        .sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
        .hinstance = GetModuleHandle(nullptr),
        .hwnd = glfwGetWin32Window(window)
    };

    VkResult r = vkCreateWin32SurfaceKHR(m_instance, &createInfo, nullptr, &m_surface);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create window surface");
    }
}

void VulkanEngine::createSwapchain(GLFWwindow* window)
{
    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(m_physicalDevice, m_surface, &surfaceCapabilities);

    VkSurfaceFormatKHR surfaceFormat = chooseSwapSurfaceFormat();
    VkPresentModeKHR presentMode = chooseSwapPresentMode();
    VkExtent2D extent = chooseSwapExtent(window);

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
    VkResult r = vkCreateSwapchainKHR(m_device, &createInfo, nullptr, &m_swapchain);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create swapchain");
    }

    vkGetSwapchainImagesKHR(m_device, m_swapchain, &imageCount, nullptr);
    m_swapchainImages.resize(imageCount);
    vkGetSwapchainImagesKHR(m_device, m_swapchain, &imageCount, m_swapchainImages.data());

    m_swapchainImageFormat = surfaceFormat;
    m_swapchainExtent = extent;
}

VkSurfaceFormatKHR VulkanEngine::chooseSwapSurfaceFormat() const
{
    uint32_t availableFormatCount = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(m_physicalDevice, m_surface, &availableFormatCount, nullptr);
    std::vector<VkSurfaceFormatKHR> availableFormats(availableFormatCount);
    vkGetPhysicalDeviceSurfaceFormatsKHR(m_physicalDevice, m_surface, &availableFormatCount, availableFormats.data());

    auto it = std::ranges::find_if(availableFormats, [](const VkSurfaceFormatKHR& availableFormat)
    {
        return availableFormat.format == VK_FORMAT_B8G8R8A8_SRGB && availableFormat.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
    });

    return it != availableFormats.end() ? *it : availableFormats[0];
}

VkPresentModeKHR VulkanEngine::chooseSwapPresentMode() const
{
    uint32_t availablePresentModeCount = 0;
    vkGetPhysicalDeviceSurfacePresentModesKHR(m_physicalDevice, m_surface, &availablePresentModeCount, nullptr);
    std::vector<VkPresentModeKHR> availablePresentModes(availablePresentModeCount);
    vkGetPhysicalDeviceSurfacePresentModesKHR(m_physicalDevice, m_surface, &availablePresentModeCount, availablePresentModes.data());

    auto it = std::ranges::find(availablePresentModes, VK_PRESENT_MODE_MAILBOX_KHR);
    return it != availablePresentModes.end() ? *it : VK_PRESENT_MODE_FIFO_KHR;
}

VkExtent2D VulkanEngine::chooseSwapExtent(GLFWwindow* window) const
{
    VkSurfaceCapabilitiesKHR  surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(m_physicalDevice, m_surface, &surfaceCapabilities);

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
        VkResult r = vkCreateImageView(m_device, &createInfo, nullptr, &imageView);
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

    VkPipelineVertexInputStateCreateInfo vertexInputInfo = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        .vertexBindingDescriptionCount = 0,
        .vertexAttributeDescriptionCount = 0
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
        .cullMode = VK_CULL_MODE_BACK_BIT,
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
        .pushConstantRangeCount = 0
    };
    VkResult r = vkCreatePipelineLayout(m_device, &pipelineLayoutInfo, nullptr, &m_pipelineLayout);
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

    r = vkCreateGraphicsPipelines(m_device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &m_graphicsPipeline);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create graphics pipeline");
    }

    vkDestroyShaderModule(m_device, module, nullptr);
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
    VkResult r = vkCreateShaderModule(m_device, &createInfo, nullptr, &shaderModule);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create shader module for file: " + filePath);
    }

    return shaderModule;
}

void VulkanEngine::createCommandPool()
{
    int graphicsQueueFamilyIndex = findQueueFamilies(m_physicalDevice);
    if (graphicsQueueFamilyIndex == -1)
    {
        throw std::runtime_error("Failed to find a queue family that supports graphics commands");
    }

    VkCommandPoolCreateInfo createInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .queueFamilyIndex = static_cast<uint32_t>(graphicsQueueFamilyIndex)
    };
    VkResult r = vkCreateCommandPool(m_device, &createInfo, nullptr, &m_commandPool);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to create command pool");
    }
}

void VulkanEngine::createCommandBuffer()
{
    VkCommandBufferAllocateInfo allocInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = m_commandPool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1
    };
    VkResult r = vkAllocateCommandBuffers(m_device, &allocInfo, &m_commandBuffer);
    if (r != VK_SUCCESS)
    {
        throw std::runtime_error("Failed to allocate command buffer");
    }
}

void VulkanEngine::recordCommandBuffer(uint32_t imageIndex) const
{
    VkCommandBufferBeginInfo beginInfo = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
    };
    VkResult r = vkBeginCommandBuffer(m_commandBuffer, &beginInfo);
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

    vkCmdBeginRendering(m_commandBuffer, &renderingInfo);

    vkCmdBindPipeline(m_commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, m_graphicsPipeline);

    vkCmdSetViewport(m_commandBuffer, 0, 1, &m_viewport);
    vkCmdSetScissor(m_commandBuffer, 0, 1, &m_scissor);

    vkCmdDraw(m_commandBuffer, 3, 1, 0, 0);

    vkCmdEndRendering(m_commandBuffer);

    transitionImageLayout(
        imageIndex,
        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        0,
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    );

    r = vkEndCommandBuffer(m_commandBuffer);
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
    vkCmdPipelineBarrier2(m_commandBuffer, &dependencyInfo);
}
