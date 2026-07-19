for ($i = 32; $i -lt 51; $i++) {
    Write-Host "-----------------------"
    Write-Host "current: mod $i"
    python ./chebyshev_bias.py $i 1000
}