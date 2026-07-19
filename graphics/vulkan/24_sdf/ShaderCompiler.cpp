#include "ShaderCompiler.h"

#include <stdexcept>
#include <iostream>

namespace
{
void diagnoseIfNeeded(const Slang::ComPtr<slang::IBlob>& diagnosticBlob)
{
    if (diagnosticBlob != nullptr)
    {
        std::cout << static_cast<const char*>(diagnosticBlob->getBufferPointer()) << std::endl;
    }
}

void checkResult(SlangResult result, const Slang::ComPtr<slang::IBlob>& diagnosticBlob, const std::string& errMsg)
{
    diagnoseIfNeeded(diagnosticBlob);
    if (SLANG_FAILED(result))
    {
        throw std::runtime_error(errMsg);
    }
}
}

ShaderCompiler::ShaderCompiler()
{
    SlangResult r = slang::createGlobalSession(m_globalSession.writeRef());
    if (SLANG_FAILED(r))
    {
        throw std::runtime_error("failed to create global session");
    }

    slang::TargetDesc target = {
        .format = SLANG_SPIRV,
        .profile = m_globalSession->findProfile("spirv_1_5")
    };
    slang::SessionDesc session = {
        .targets = &target,
        .targetCount = 1
    };

    m_globalSession->createSession(session, m_session.writeRef());
}

ShaderCompiler::~ShaderCompiler() = default;

std::vector<std::byte> ShaderCompiler::compile(const std::string& filePath) const
{
    Slang::ComPtr<slang::IBlob> diagnosticBlob;

    Slang::ComPtr<slang::IModule> module;
    module = m_session->loadModule(filePath.c_str(), diagnosticBlob.writeRef());
    diagnoseIfNeeded(diagnosticBlob);
    if (!module)
    {
        throw std::runtime_error("failed to load module");
    }

    std::vector<Slang::ComPtr<slang::IComponentType>> components;

    int entryPointCount = module->getDefinedEntryPointCount();
    for (int i = 0; i < entryPointCount; i++)
    {
        Slang::ComPtr<slang::IEntryPoint> entry;
        SlangResult r = module->getDefinedEntryPoint(i, entry.writeRef());
        if (SLANG_FAILED(r))
        {
            throw std::runtime_error("failed to get entry point");
        }

        components.emplace_back(entry.get());
    }

    Slang::ComPtr<slang::IComponentType> composedProgram;
    SlangResult r = m_session->createCompositeComponentType(
        reinterpret_cast<slang::IComponentType* const*>(components.data()),
        components.size(),
        composedProgram.writeRef(),
        diagnosticBlob.writeRef()
    );
    checkResult(r, diagnosticBlob, "failed to composite program");

    Slang::ComPtr<slang::IComponentType> program;
    r = composedProgram->link(program.writeRef(), diagnosticBlob.writeRef());
    checkResult(r, diagnosticBlob, "failed to link program");

    Slang::ComPtr<slang::IBlob> code;
    r = program->getEntryPointCode(0, 0, code.writeRef(), diagnosticBlob.writeRef());
    checkResult(r, diagnosticBlob, "failed to get code");

    std::vector<std::byte> out(code->getBufferSize());
    memcpy(out.data(), code->getBufferPointer(), code->getBufferSize());

    return out;
}
