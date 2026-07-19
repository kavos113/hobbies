module "1_color";

public float4 colorByDistance(float distance)
{
    float4 negativeColor = float4(0.12, 0.92, 0.33, 1.0);
    float4 positiveColor = float4(0.63, 0.41, 0.18, 1.0);

    float4 outColor;

    if (abs(distance) < 2)
    {
        outColor = float4(1.0, 1.0, 1.0, 1.0);
    }
    else
    {
        outColor = distance > 0.0 ? positiveColor * (distance / 100) : negativeColor * (distance / -100);
    }

    return outColor;
}
