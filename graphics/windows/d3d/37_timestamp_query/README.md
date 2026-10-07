# timestamp query

GPUコマンドの実行時間を計測する

### 流れ

必要なリソース
- QueryHeap
- Query Result Buffer: QueryHeapにある結果を読み取るためのバッファ

処理の流れ
- EndQueryでタイムスタンプクエリを実行
- ResolveQueryDataでbufferに転送
- それをCPUから読む

### 初期化

```c++ 
D3D12_QUERY_HEAP_DESC desc = {
    .Type = D3D12_QUERY_HEAP_TYPE_TIMESTAMP,
    .Count = 2,
};
m_context->device()->CreateQueryHeap(&desc, IID_PPV_ARGS(&m_queryHeap));

m_context->buffer()->createBuffer(
    D3DBuffer::ResourceDesc_Buffer(16), // uint64 * 2
    D3D12_HEAP_TYPE_READBACK,
    D3D12_RESOURCE_STATE_COPY_DEST,
    &m_queryResult
);
```

Query Result HeapにはREADBACKを使う

### 処理
queryをGPUコマンドとして記録
```c++
m_commandList->EndQuery(m_queryHeap.Get(), D3D12_QUERY_TYPE_TIMESTAMP, 0);

m_model->render(m_commandList);

m_commandList->EndQuery(m_queryHeap.Get(), D3D12_QUERY_TYPE_TIMESTAMP, 1);

m_commandList->ResolveQueryData(
    m_queryHeap.Get(),
    D3D12_QUERY_TYPE_TIMESTAMP,
    0, 2,
    m_queryResult->GetResource(),
    0
);
```

GPUコマンド実行後に読む
```c++
uint64_t *queryResult = nullptr;
m_queryResult->GetResource()->Map(
    0,
    nullptr,
    reinterpret_cast<void**>(&queryResult)
);

uint64_t start = *queryResult;
uint64_t end = *(queryResult + 1);

UINT64 frequency;
m_commandQueue->GetTimestampFrequency(&frequency);
```