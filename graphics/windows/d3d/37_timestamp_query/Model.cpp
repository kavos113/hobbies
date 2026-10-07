#include "Model.h"

#include <DirectXTex.h>

#define TINYOBJLOADER_IMPLEMENTATION
#include <tiny_obj_loader.h>

#include <iostream>
#include <map>

Model::Model(
    RECT rc,
    D3DContext *context,
    DescriptorHeapManager *descHeapManager
) : m_context(context),
    m_descHeapManager(descHeapManager)
{
    loadModel(MODEL_PATH);
    createVertexBuffer();
    createIndexBuffer();

    createMatrixBuffer(rc);
    createLightBuffer();

    loadTexture(TEXTURE_PATH);
}

void Model::cleanup()
{
    m_vertexBuffer.Reset();
    m_indexBuffer.Reset();
    m_texture.Reset();
    m_matrixBuffer.Reset();
    m_lightBuffer.Reset();
}

void Model::render(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &commandList)
{
    m_angle += 0.01f;
    DirectX::XMMATRIX world = DirectX::XMMatrixRotationY(m_angle);
    m_matrixBufferData->world = world;

    commandList->IASetIndexBuffer(&m_indexBufferView);
    commandList->IASetVertexBuffers(0, 1, &m_vertexBufferView);

    commandList->DrawIndexedInstanced(m_indices.size(), 1, 0, 0, 0);
    // for (int i = 0; i < 100; i++)
    // {
    //     commandList->DrawIndexedInstanced(m_indices.size(), 1, 0, 0, 0);
    // }
}

void Model::executeBarrier(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList>& commandList) const
{
    commandList->ResourceBarrier(
        m_barriers.size(),
        m_barriers.data()
    );
}

void Model::loadModel(const std::string &path)
{
    tinyobj::ObjReaderConfig readerConfig;
    readerConfig.mtl_search_path = ".";

    tinyobj::ObjReader reader;

    if (!reader.ParseFromFile(path, readerConfig))
    {
        if (!reader.Error().empty())
        {
            std::cerr << "Failed to load model: " << reader.Error() << std::endl;
        }
        return;
    }

    if (!reader.Warning().empty())
    {
        std::cout << "Model load warning: " << reader.Warning() << std::endl;
    }

    auto& attrib = reader.GetAttrib();
    auto& shapes = reader.GetShapes();

    std::map<Vertex, unsigned short> uniqueVertices;

    for (const auto & shape : shapes)
    {
        size_t indexOffset = 0;
        for (size_t f = 0; f < shape.mesh.num_face_vertices.size(); ++f)
        {
            size_t fv = shape.mesh.num_face_vertices[f];
            for (size_t v = 0; v < fv; ++v)
            {
                tinyobj::index_t idx = shape.mesh.indices[indexOffset + v];
                Vertex vertex = {};
                vertex.position = {
                    attrib.vertices[3 * idx.vertex_index + 0],
                    attrib.vertices[3 * idx.vertex_index + 1],
                    attrib.vertices[3 * idx.vertex_index + 2]
                };

                if (idx.normal_index >= 0)
                {
                    vertex.normal = {
                        attrib.normals[3 * idx.normal_index + 0],
                        attrib.normals[3 * idx.normal_index + 1],
                        attrib.normals[3 * idx.normal_index + 2]
                    };
                }

                if (idx.texcoord_index >= 0)
                {
                    vertex.uv = {
                        attrib.texcoords[2 * idx.texcoord_index + 0],
                        1.0f - attrib.texcoords[2 * idx.texcoord_index + 1]
                    };
                }

                if (!uniqueVertices.contains(vertex))
                {
                    uniqueVertices[vertex] = static_cast<unsigned short>(m_vertices.size());
                    m_vertices.push_back(vertex);
                }

                m_indices.push_back(uniqueVertices[vertex]);
            }
            indexOffset += fv;
        }
    }
}

void Model::createVertexBuffer()
{
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(sizeof(Vertex) * m_vertices.size()),
        D3D12_HEAP_TYPE_DEFAULT,
        D3D12_RESOURCE_STATE_COMMON,
        &m_vertexBuffer
    );

    Microsoft::WRL::ComPtr<D3D12MA::Allocation> stagingBuffer;
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(sizeof(Vertex) * m_vertices.size()),
        D3D12_HEAP_TYPE_UPLOAD,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        &stagingBuffer
    );

    Vertex *vertexMap = nullptr;
    HRESULT hr = stagingBuffer->GetResource()->Map(
        0,
        nullptr,
        reinterpret_cast<void**>(&vertexMap)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to map vertex buffer." << std::endl;
        return;
    }
    std::ranges::copy(m_vertices, vertexMap);
    stagingBuffer->GetResource()->Unmap(0, nullptr);

    m_context->buffer()->copyBuffer(stagingBuffer, m_vertexBuffer);

    m_vertexBufferView = {
        .BufferLocation = m_vertexBuffer->GetResource()->GetGPUVirtualAddress(),
        .SizeInBytes = static_cast<UINT>(sizeof(Vertex) * m_vertices.size()),
        .StrideInBytes = sizeof(Vertex)
    };

    barrier(
        m_vertexBuffer,
        D3D12_RESOURCE_STATE_COPY_DEST,
        D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER
    );

    m_context->buffer()->registerWaitForCopyResource(stagingBuffer);
}

void Model::createIndexBuffer()
{
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(sizeof(unsigned short) * m_indices.size()),
        D3D12_HEAP_TYPE_DEFAULT,
        D3D12_RESOURCE_STATE_COMMON,
        &m_indexBuffer
    );

    Microsoft::WRL::ComPtr<D3D12MA::Allocation> stagingBuffer;
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(sizeof(unsigned short) * m_indices.size()),
        D3D12_HEAP_TYPE_UPLOAD,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        &stagingBuffer
    );

    unsigned short *indexMap = nullptr;
    HRESULT hr = stagingBuffer->GetResource()->Map(
        0,
        nullptr,
        reinterpret_cast<void**>(&indexMap)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to map index buffer." << std::endl;
        return;
    }
    std::ranges::copy(m_indices, indexMap);
    stagingBuffer->GetResource()->Unmap(0, nullptr);

    m_context->buffer()->copyBuffer(stagingBuffer, m_indexBuffer);

    m_indexBufferView = {
        .BufferLocation = m_indexBuffer->GetResource()->GetGPUVirtualAddress(),
        .SizeInBytes = static_cast<UINT>(sizeof(unsigned short) * m_indices.size()),
        .Format = DXGI_FORMAT_R16_UINT
    };

    barrier(
        m_indexBuffer,
        D3D12_RESOURCE_STATE_COPY_DEST,
        D3D12_RESOURCE_STATE_INDEX_BUFFER
    );

    m_context->buffer()->registerWaitForCopyResource(stagingBuffer);
}

void Model::createMatrixBuffer(RECT rc)
{
    DirectX::XMMATRIX world = DirectX::XMMatrixIdentity();
    DirectX::XMMATRIX view = DirectX::XMMatrixLookAtLH(
        { 0.0f, 0.2f, -0.5f },
        { 0.0f, 0.0f, 0.0f },
        { 0.0f, 1.0f, 0.0f }
    );
    DirectX::XMMATRIX projection = DirectX::XMMatrixPerspectiveFovLH(
        DirectX::XM_PIDIV2,
        static_cast<float>(rc.right - rc.left) / static_cast<float>(rc.bottom - rc.top),
        0.1f,
        100.0f
    );

    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(AlignCBuffer(sizeof(MatrixBuffer))),
        D3D12_HEAP_TYPE_UPLOAD,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        &m_matrixBuffer
    );

    HRESULT hr = m_matrixBuffer->GetResource()->Map(
        0,
        nullptr,
        reinterpret_cast<void**>(&m_matrixBufferData)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to map matrix buffer." << std::endl;
        return;
    }

    m_matrixBufferData->world = world;
    m_matrixBufferData->view = view;
    m_matrixBufferData->projection = projection;

    D3D12_CPU_DESCRIPTOR_HANDLE cbvHandle = m_descHeapManager->cbvHeap()->allocate(1);
    D3D12_CONSTANT_BUFFER_VIEW_DESC cbvDesc = {
        .BufferLocation = m_matrixBuffer->GetResource()->GetGPUVirtualAddress(),
        .SizeInBytes = AlignCBuffer(sizeof(MatrixBuffer))
    };

    m_context->device()->CreateConstantBufferView(
        &cbvDesc,
        cbvHandle
    );

    m_descHeapManager->cbvHeapManager()->setHandle(cbvHandle, 0, DescriptorBindingManager::VS_CBV);
}

void Model::createLightBuffer()
{
    DirectX::XMFLOAT3 direction{-1.0f, -3.0f, 1.0f};
    DirectX::XMFLOAT3 ambient{0.3f, 0.3f, 0.3f};

    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(AlignCBuffer(sizeof(LightBuffer))),
        D3D12_HEAP_TYPE_DEFAULT,
        D3D12_RESOURCE_STATE_COMMON,
        &m_lightBuffer
    );

    Microsoft::WRL::ComPtr<D3D12MA::Allocation> stagingBuffer;
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(AlignCBuffer(sizeof(LightBuffer))),
        D3D12_HEAP_TYPE_UPLOAD,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        &stagingBuffer
    );

    LightBuffer *map = nullptr;
    HRESULT hr = stagingBuffer->GetResource()->Map(
        0,
        nullptr,
        reinterpret_cast<void**>(&map)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to map light buffer." << std::endl;
        return;
    }
    map->direction = direction;
    map->ambient = ambient;
    stagingBuffer->GetResource()->Unmap(0, nullptr);

    m_context->buffer()->copyBuffer(stagingBuffer, m_lightBuffer);

    m_context->buffer()->registerWaitForCopyResource(stagingBuffer);

    barrier(
        m_lightBuffer,
        D3D12_RESOURCE_STATE_COPY_DEST,
        D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER
    );

    D3D12_CPU_DESCRIPTOR_HANDLE cbvHandle = m_descHeapManager->cbvHeap()->allocate(1);
    D3D12_CONSTANT_BUFFER_VIEW_DESC cbvDesc = {
        .BufferLocation = m_lightBuffer->GetResource()->GetGPUVirtualAddress(),
        .SizeInBytes = AlignCBuffer(sizeof(LightBuffer))
    };
    m_context->device()->CreateConstantBufferView(
        &cbvDesc,
        cbvHandle
    );

    m_descHeapManager->cbvHeapManager()->setHandle(cbvHandle, 1, DescriptorBindingManager::PS_CBV);
}

void Model::loadTexture(const std::wstring &path)
{
    DirectX::TexMetadata metadata{};
    DirectX::ScratchImage scratchImage;

    HRESULT hr = DirectX::LoadFromWICFile(
        path.c_str(),
        DirectX::WIC_FLAGS_NONE,
        &metadata,
        scratchImage
    );
    if (FAILED(hr))
    {
        if (hr == HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND))
        {
            std::wcerr << L"Texture file not found: " << path << std::endl;
        }
        else
        {
            std::wcerr << L"Failed to load texture from file: " << path << L" with error: " << std::hex << hr << std::endl;
        }
        return;
    }

    const DirectX::Image *image = scratchImage.GetImage(0, 0, 0);

    D3D12MA::ALLOCATION_DESC allocDesc = {};
    allocDesc.HeapType = D3D12_HEAP_TYPE_DEFAULT;

    D3D12_RESOURCE_DESC resourceDesc = {
        .Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D,
        .Alignment = 0,
        .Width = metadata.width,
        .Height = static_cast<UINT>(metadata.height),
        .DepthOrArraySize = static_cast<UINT16>(metadata.arraySize),
        .MipLevels = static_cast<UINT16>(metadata.mipLevels),
        .Format = metadata.format,
        .SampleDesc = {1, 0},
        .Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN,
        .Flags = D3D12_RESOURCE_FLAG_NONE
    };
    hr = m_context->allocator()->CreateResource(
        &allocDesc,
        &resourceDesc,
        D3D12_RESOURCE_STATE_COMMON,
        nullptr,
        &m_texture,
        IID_NULL,
        nullptr
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create texture resource." << std::endl;
        return;
    }

    Microsoft::WRL::ComPtr<D3D12MA::Allocation> stagingResource;
    m_context->buffer()->createBuffer(
        D3DBuffer::ResourceDesc_Buffer(AlignCBuffer(image->rowPitch) * image->height),
        D3D12_HEAP_TYPE_UPLOAD,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        &stagingResource
    );

    uint8_t *mappedData = nullptr;
    hr = stagingResource->GetResource()->Map(
        0,
        nullptr,
        reinterpret_cast<void**>(&mappedData)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to map staging resource for texture upload." << std::endl;
        return;
    }

    for (UINT y = 0; y < metadata.height; ++y)
    {
        std::memcpy(
            mappedData + y * image->rowPitch,
            image->pixels + y * image->rowPitch,
            image->rowPitch
        );
    }
    stagingResource->GetResource()->Unmap(0, nullptr);

    m_context->buffer()->copyTexture(stagingResource, m_texture);

    barrier(
        m_texture,
        D3D12_RESOURCE_STATE_COPY_DEST,
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE
    );

    m_context->buffer()->registerWaitForCopyResource(stagingResource);

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc = {
        .Format = metadata.format,
        .ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D,
        .Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING,
        .Texture2D = {
            .MostDetailedMip = 0,
            .MipLevels = static_cast<UINT>(metadata.mipLevels),
            .PlaneSlice = 0,
            .ResourceMinLODClamp = 0.0f
        }
    };

    D3D12_CPU_DESCRIPTOR_HANDLE srvHandle = m_descHeapManager->cbvHeap()->allocate(1);

    m_context->device()->CreateShaderResourceView(
        m_texture->GetResource(),
        &srvDesc,
        srvHandle
    );

    m_descHeapManager->cbvHeapManager()->setHandle(srvHandle, 0, DescriptorBindingManager::PS_SRV);
}

void Model::barrier(
    const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &resource,
    D3D12_RESOURCE_STATES beforeState,
    D3D12_RESOURCE_STATES afterState
)
{
    m_barriers.push_back(
        D3D12_RESOURCE_BARRIER{
            .Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
            .Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE,
            .Transition = {
                .pResource = resource->GetResource(),
                .Subresource = 0,
                .StateBefore = beforeState,
                .StateAfter = afterState
            }
        }
    );
}
