$packageDirs = Get-ChildItem -Path . -Filter "Cargo.toml" -Recurse -ErrorAction SilentlyContinue |
Where-Object { $_.FullName -notmatch "target" } |
Select-Object -ExpandProperty DirectoryName

foreach ($dir in $packageDirs) {
    Write-Host "----Updating dependencies in $dir ----" -ForegroundColor Cyan

    Push-Location $dir

    try {
        cargo update
    }
    catch {
        Write-Error "Failed to update dependencies in $dir"
    }
    finally {
        Pop-Location
    }
}

Write-Host "---- Package update process completed ----" -ForegroundColor Green