struct Data
{
    float id;
    float value;
};

RWStructuredBuffer<Data> data;

[shader("compute")]
[numthreads(8, 8, 8)]
void compMain(uint3 threadId: SV_DispatchThreadID)
{
    Data data;
    data.id = threadId.x;
}