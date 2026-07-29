Param(
    $Dirpath
)

$exts = Get-ChildItem $Dirpath -File -Recurse | ForEach-Object -Process {
    return $_.Extension.ToLower()
}

Write-Host "total" $exts.Length "files"

$uni = $exts | Sort-Object | Get-Unique

Write-Host "total" $uni.Length "extensions:"
$uni | ForEach-Object -Process {
    Write-Host $_
}