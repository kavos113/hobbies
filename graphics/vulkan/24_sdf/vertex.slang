module vertex;

public struct VertexInput
{
    public float2 position;
    public float3 color;
};

public struct VertexOutput
{
    public float4 pos : SV_Position;
    public float3 color;
};

public struct PushConstant
{
    public float4 data1;
    public float4 data2;
    public float4 data3;
    public float4 data4;
};
[[vk::push_constant]]
public ConstantBuffer<PushConstant> push;

[shader("vertex")]
VertexOutput vertMain(VertexInput input)
{
    VertexOutput output;
    output.pos = float4(input.position, 0.0, 1.0);
    output.color = input.color;

    return output;
}