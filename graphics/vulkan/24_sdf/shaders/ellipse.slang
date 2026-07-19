import "../vertex";
import "1_color";

/*
Push Constant Layout:

data1: center (2) | radius (2)

*/

float sdf(float2 pos)
{
    float2 c = push.data1.xy;
    float2 r = push.data1.zw;
    float2 p = pos - c;

    return (dot(p / r, p / r) - 1.0) / max(length(p / (r * r)) * 2.0, 1e-5);
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float dis = sdf(vertInput.pos.xy);
    return colorByDistance(dis);
}
