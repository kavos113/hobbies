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
    PushConstant push;
};

inline const std::vector<SDFEntry> ENTRIES = {
    SDFEntry{
        .shaderFile = "shaders/0_base.slang",
        .push = {}
    }
};

#endif //SDF_SHADERS_ENTRIES_H
