import "../vertex";
import "1_color";

/*
Push Constant Layout:

data1: center (2) | (center -> bottom right) vector (2)
data2: corner radius (2)

*/

float sdf(float2 pos)
{
    float2 c = push.data1.xy;
    float2 v = push.data1.zw;
    float2 r = push.data2.xy;

    float2 s = abs(pos - c) - v;
    float2 t = s / r;
    float2 sr = t + 1.0;

    return lerp(max(t.x, t.y), length(sr) - 1.0, step(0.0, min(sr.x, sr.y))) * length(r);
}

[shader("fragment")]
float4 fragMain(VertexOutput vertInput) : SV_Target
{
    float dis = sdf(vertInput.pos.xy);
    return colorByDistance(dis);
}
