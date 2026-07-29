$packageDirs = Get-ChildItem -Path . -Filter "go.mod" -Recurse -ErrorAction SilentlyContinue |
Select-Object -ExpandProperty DirectoryName

foreach ($dir in $packageDirs) {
    Write-Host "----Updating packages in $dir ----" -ForegroundColor Cyan

    Push-Location $dir

    try {
        go get -u
        Write-Host "> go mod tidy" -ForegroundColor Gray
        go mod tidy
    }
    catch {
        Write-Error "Failed to update packages in $dir"
    }
    finally {
        Pop-Location
    }
}

Write-Host "---- Package update process completed ----" -ForegroundColor Green