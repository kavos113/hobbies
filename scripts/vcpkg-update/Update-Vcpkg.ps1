$vcpkg_dir = (Get-Command vcpkg).Source | Split-Path -Parent

Write-Host "found vcpkg: $vcpkg_dir"

Push-Location $vcpkg_dir
    git pull
Pop-Location