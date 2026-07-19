import "../vertex";
import "1_color";

/*
Push Constant Layout:

data1: center (2) | (center -> bottom right) vector (2)

*/

float sdf(float2 pos)
{
    float2 c = push.data1.xy;
    float2 v = push.data1.zw;

    float2 s = abs(pos - c) - v;
    return length(max(s, 0.0)) + min(max(s.x, s.y), 0.0);
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float dis = sdf(vertInput.pos.xy);
    return colorByDistance(dis);
}
