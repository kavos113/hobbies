Param($command)

function New-Sources {
    $dirs = Get-ChildItem -Path . -Attributes D

    foreach ($dir in $dirs) {
        Push-Location $dir

        gcc -o main main.c -std=c11
        objdump -d -M intel main.exe > main.asm

        Write-Host "build successfully: $dir"

        Pop-Location
    }    
}

function Clear-Sources {
    $dirs = Get-ChildItem -Path . -Attributes D

    foreach ($dir in $dirs) {
        Push-Location $dir

        Remove-Item -Path main.exe, main.asm
        Pop-Location
    }    
}

if ($command -eq "build") {
    New-Sources
}
elseif ($command -eq "clean") {
    Clear-Sources
}
else {
    Write-Host "unknown command: $command. available: build / clean"
}