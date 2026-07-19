import "../vertex";
import "1_color";

/*
Push Constant Layout:

data1: center (2) | radius

*/

float sdf(float2 pos)
{
    float2 center = push.data1.xy;
    float radius = push.data1.z;

    return distance(pos, center) - radius;
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float dis = sdf(vertInput.pos.xy);
    return colorByDistance(dis);
}
