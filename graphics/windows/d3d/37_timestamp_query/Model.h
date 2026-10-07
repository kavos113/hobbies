#ifndef MODEL_H
#define MODEL_H

#include <d3d12.h>
#include <DirectXMath.h>
#include <wrl/client.h>

#include <vector>
#include <array>
#include <D3D12MemAlloc.h>
#include <string>

#include "D3DContext.h"
#include "D3DDescriptorHeap.h"

class Model
{
public:
    Model(
        RECT rc,
        D3DContext *context,
        DescriptorHeapManager *descHeapManager
    );
    void cleanup();
    void executeBarrier(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList>& commandList) const;

    void render(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &commandList);

    struct Vertex
    {
        DirectX::XMFLOAT3 position;
        DirectX::XMFLOAT3 normal;
        DirectX::XMFLOAT2 uv;

        static auto inputLayout()
        {
            return std::array{
                D3D12_INPUT_ELEMENT_DESC{
                    .SemanticName = "POSITION",
                    .SemanticIndex = 0,
                    .Format = DXGI_FORMAT_R32G32B32_FLOAT,
                    .InputSlot = 0,
                    .AlignedByteOffset = 0,
                    .InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                    .InstanceDataStepRate = 0
                },
                D3D12_INPUT_ELEMENT_DESC{
                    .SemanticName = "NORMAL",
                    .SemanticIndex = 0,
                    .Format = DXGI_FORMAT_R32G32B32_FLOAT,
                    .InputSlot = 0,
                    .AlignedByteOffset = D3D12_APPEND_ALIGNED_ELEMENT,
                    .InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                    .InstanceDataStepRate = 0
                },
                D3D12_INPUT_ELEMENT_DESC{
                    .SemanticName = "TEXCOORD",
                    .SemanticIndex = 0,
                    .Format = DXGI_FORMAT_R32G32_FLOAT,
                    .InputSlot = 0,
                    .AlignedByteOffset = D3D12_APPEND_ALIGNED_ELEMENT,
                    .InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                    .InstanceDataStepRate = 0
                }
            };
        }

        bool operator< (const Vertex &other) const
        {
            return position.x < other.position.x ||
                   (position.x == other.position.x && position.y < other.position.y) ||
                   (position.x == other.position.x && position.y == other.position.y && position.z < other.position.z) ||
                   (position.x == other.position.x && position.y == other.position.y && position.z == other.position.z && uv.x < other.uv.x) ||
                   (position.x == other.position.x && position.y == other.position.y && position.z == other.position.z && uv.x == other.uv.x && uv.y < other.uv.y);
        }
    };

private:

    struct MatrixBuffer
    {
        DirectX::XMMATRIX world;
        DirectX::XMMATRIX view;
        DirectX::XMMATRIX projection;
    };

    struct LightBuffer
    {
        DirectX::XMFLOAT3 direction;
        float padding;
        DirectX::XMFLOAT3 ambient;
    };

    void loadModel(const std::string &path);
    void createVertexBuffer();
    void createIndexBuffer();
    void createMatrixBuffer(RECT rc);
    void createLightBuffer();

    void loadTexture(const std::wstring& path);

    void barrier(
        const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &resource,
        D3D12_RESOURCE_STATES beforeState,
        D3D12_RESOURCE_STATES afterState
    );

    D3DContext *m_context;
    DescriptorHeapManager *m_descHeapManager;

    std::vector<Vertex> m_vertices;
    std::vector<unsigned short> m_indices;

    D3D12_VERTEX_BUFFER_VIEW m_vertexBufferView = {};
    Microsoft::WRL::ComPtr<D3D12MA::Allocation> m_vertexBuffer;
    D3D12_INDEX_BUFFER_VIEW m_indexBufferView = {};
    Microsoft::WRL::ComPtr<D3D12MA::Allocation> m_indexBuffer;

    float m_angle = 0.0f;
    Microsoft::WRL::ComPtr<D3D12MA::Allocation> m_matrixBuffer;
    MatrixBuffer *m_matrixBufferData = nullptr;

    Microsoft::WRL::ComPtr<D3D12MA::Allocation> m_lightBuffer;
    Microsoft::WRL::ComPtr<D3D12MA::Allocation> m_texture;

    std::vector<D3D12_RESOURCE_BARRIER> m_barriers;

    const std::string MODEL_PATH = "security_camera_01_2k.obj";
    const std::wstring TEXTURE_PATH = L"textures/security_camera_01_diff_2k.jpg";
};



#endif //MODEL_H
