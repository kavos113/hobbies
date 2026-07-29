$count = 0

$exts = git ls-files | Get-Item | Where-Object {
    $count++
    if ($count % 100 -eq 0) {
        Write-Host "`rProcessed $count files" -NoNewline
    }

    if ($_.Length -eq 0) { return $true }
    $bytes = [System.IO.File]::ReadAllBytes($_.FullName) | Select-Object -First 1024
    return -not ($bytes -contains 0)
} | ForEach-Object -Process {
    return $_.Extension.ToLower()
}

Write-Host "`ntotal" $exts.Length "files"

$uni = $exts | Sort-Object | Get-Unique

Write-Host "total" $uni.Length "extensions:"
$uni | ForEach-Object -Process {
    Write-Host $_
}