#ifndef SDF_SHADER_H
#define SDF_SHADER_H

#include <cstddef>

#include <vector>
#include <string>

#include <slang.h>
#include <slang-com-ptr.h>

class Shader
{
public:
    Shader();
    ~Shader();

    std::vector<std::byte> compile(const std::string& filePath) const;

private:
    Slang::ComPtr<slang::IGlobalSession> m_globalSession;
    Slang::ComPtr<slang::ISession> m_session;
};


#endif // SDF_SHADER_H
