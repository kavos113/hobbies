$packageDirs = Get-ChildItem -Path . -Filter "package.json" -Recurse -ErrorAction SilentlyContinue |
Where-Object { $_.FullName -notmatch "node_modules" } |
Select-Object -ExpandProperty DirectoryName

foreach ($dir in $packageDirs) {
    Write-Host "----Updating packages in $dir ----" -ForegroundColor Cyan

    Push-Location $dir

    try {
        if (Test-Path "package-lock.json") {
            npm audit fix --force
            npm install
        }
        elseif (Test-Path "pnpm-lock.yaml") {
            pnpm audit --fix
            pnpm up --lockfile-only
        }
        else {
            Write-Warning "No recognized lock file found in $dir. Skipping package update."
        }
    }
    catch {
        Write-Error "Failed to update packages in $dir"
    }
    finally {
        Pop-Location
    }
}

Write-Host "---- Package update process completed ----" -ForegroundColor Green