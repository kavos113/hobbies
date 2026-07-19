module "1_color";

public float4 colorByDistance(float distance)
{
    float4 negativeColor = float4(0.12, 0.92, 0.33, 1.0);
    float4 positiveColor = float4(0.63, 0.41, 0.18, 1.0);

    float4 color = (positiveColor * max(distance, 0.0) / 100.0) + (negativeColor * max(-distance, 0.0) / 100.0);
    return lerp(float4(1, 1, 1, 0), color, step(1.0, abs(distance)));
}
