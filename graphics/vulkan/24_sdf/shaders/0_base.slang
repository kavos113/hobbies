import "../vertex";

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float3 color = vertInput.color;
    return float4(color, 1.0);
}
