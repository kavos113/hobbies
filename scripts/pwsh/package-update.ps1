$packageDirs = Get-ChildItem -Path . -Filter "package.json" -Recurse -ErrorAction SilentlyContinue |
Where-Object { $_.FullName -notmatch "node_modules" } |
Select-Object -ExpandProperty DirectoryName

foreach ($dir in $packageDirs) {
    Write-Host "----Updating packages in $dir ----" -ForegroundColor Cyan

    Push-Location $dir

    try {
        pnpm up --lockfile-only
    }
    catch {
        Write-Error "Failed to update packages in $dir"
    }
    finally {
        Pop-Location
    }
}

Write-Host "---- Package update process completed ----" -ForegroundColor Green