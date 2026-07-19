import "../vertex";
import "1_color";

/*
Push Constant Layout:

data1: center (2) | radius (2)

*/

float sdf(float2 p)
{
    float2 c = push.data1.xy;
    float2 r = push.data1.zw;

    return (length((pos - c) / r) - 1.0) * length(r);
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float dis = sdf(vertInput.pos.xy);
    return colorByDistance(dis);
}
