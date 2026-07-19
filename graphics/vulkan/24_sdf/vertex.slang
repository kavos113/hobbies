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

[shader("vertex")]
VertexOutput vertMain(VertexInput input)
{
    VertexOutput output;
    output.pos = float4(input.position, 0.0, 1.0);
    output.color = input.color;

    return output;
}