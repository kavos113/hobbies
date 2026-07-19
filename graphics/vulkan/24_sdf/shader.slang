struct VertexInput
{
    float2 position;
    float3 color;
};

struct UniformBuffer
{
    float4x4 model;
    float4x4 view;
    float4x4 proj;
};
ConstantBuffer<UniformBuffer> ubo;

struct VertexOutput
{
    float4 sv_position : SV_Position;
    float3 color;
};

[shader("vertex")]
VertexOutput vertMain(VertexInput input)
{
    VertexOutput output;
    output.sv_position = mul(ubo.proj, mul(ubo.view, mul(ubo.model, float4(input.position, 0.0, 1.0))));
    output.color = input.color;

    return output;
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float3 color = vertInput.color;
    return float4(color, 1.0);
}