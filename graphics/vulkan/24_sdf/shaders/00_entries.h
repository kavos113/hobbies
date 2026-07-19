#ifndef SDF_SHADERS_ENTRIES_H
#define SDF_SHADERS_ENTRIES_H

#include <string>
#include <vector>

#include <glm/glm.hpp>
#include <vulkan/vulkan.h>

struct PushConstant
{
    glm::vec4 data1;
    glm::vec4 data2;
    glm::vec4 data3;
    glm::vec4 data4;

    static VkPushConstantRange pushConstantRange()
    {
        return {
            .stageFlags = VK_SHADER_STAGE_ALL,
            .offset = 0,
            .size = sizeof(PushConstant)
        };
    }
};

struct SDFEntry
{
    std::string shaderFile;
    std::string title;
    PushConstant push;
};

inline const std::vector<SDFEntry> ENTRIES = {
    SDFEntry{
        .shaderFile = "shaders/0_base.slang",
        .title = "base rectangle",
        .push = {}
    },
    SDFEntry{
        .shaderFile = "shaders/circle.slang",
        .title = "circle",
        .push = {
            .data1 = {400.0f, 300.0f, 100.0f, 0.0f}
        }
    },
    SDFEntry{
        .shaderFile = "shaders/rect.slang",
        .title = "rectangle",

        // actual rectangle (left, top, right, bottom) = (100, 100, 400, 300)
        .push = {
            .data1 = {250.0f, 200.0f, 150.0f, 100.0f}
        }
    },
    SDFEntry{
        .shaderFile = "shaders/ellipse.slang",
        .title = "ellipse",
        .push = {
            .data1 = {400.0f, 300.0f, 200.0f, 100.0f}
        }
    },
    SDFEntry{
        .shaderFile = "shaders/roundedrect.slang",
        .title = "rounded rectangle",

        // actual rectangle (left, top, right, bottom) = (100, 100, 400, 300)
        .push = {
            .data1 = {250.0f, 200.0f, 150.0f, 100.0f},
            .data2 = {40.0f, 20.0f, 0.0f, 0.0f}
        }
    }
};

#endif //SDF_SHADERS_ENTRIES_H
