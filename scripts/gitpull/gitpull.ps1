Param($directory)

$e = [char]27
$logLines = 5

$gitDirs = Get-ChildItem -Path $directory -Directory |
    Where-Object { Test-Path -Path (Join-Path $_.FullName ".git") }

Write-Host "total: $($gitDirs.Count) repositories found." -ForegroundColor Green

foreach ($dir in $gitDirs) {
    Write-Host "Pulling:        $($dir.Name)" -ForegroundColor Cyan
}

for ($i = 1; $i -le $logLines; $i++) {
    Write-Host ""
}

$jobs = foreach ($dir in $gitDirs) {
    Start-Job -ScriptBlock {
        param($path, $name)

        Set-Location -Path $path

        $remote = git remote
        if ([string]::IsNullOrWhiteSpace($remote)) {
            return @{
                Name = $name;
                Output = "No remote configured.";
                Success = $false;
                ExistRemote = $false
            }
        }

        $res = git pull 2>&1 | Out-String
        $success = ($LASTEXITCODE -eq 0)

        return @{
            Name = $name;
            Output = $res.Trim();
            Success = $success;
            ExistRemote = $true
        }
    } -ArgumentList $dir.FullName, $dir.Name
}

$completedTasks = @()
$recentLogs = New-Object System.Collections.Generic.List[string]

while ($completedTasks.Count -lt $gitDirs.Count) {
    $update = $false

    foreach ($job in $jobs) {
        if ($job.State -eq "Completed" -and $job.Id -notin $completedTasks) {
            $result = Receive-Job -Job $job
            $completedTasks += $job.Id

            $index = [array]::IndexOf($gitDirs.Name, $result.Name)
            $upCount = $gitDirs.Count - $index + $logLines

            Write-Host -NoNewLine "$($e)[$($upCount)A`r$($e)[K"
            if (-not $result.ExistRemote) {
                Write-Host -NoNewLine "No Remote:      $($result.Name)" -ForegroundColor Yellow
            } elseif ($result.Success) {
                Write-Host -NoNewLine "Pull Completed: $($result.Name)" -ForegroundColor Green
            } else {
                Write-Host -NoNewLine "Pull Failed:    $($result.Name)" -ForegroundColor Red
            }
            Write-Host -NoNewLine "$($e)[$($upCount)B"

            # update log
            $outstr = [string]$result.Output
            $firstLine = ""
            if (-not [string]::IsNullOrWhiteSpace($outstr)) {
                $firstLine = $outstr.Split("[\r\n]")[0].Trim()
            } else {
                $firstLine = "No output from git pull."
            }
            $recentLogs.Add("[$($result.Name)] $firstLine")
            if ($recentLogs.Count -gt $logLines) {
                $recentLogs.RemoveAt(0)
            }
            $update = $true
        }
    }

    # update log display
    if ($update) {
        Write-Host -NoNewLine "$($e)[$($logLines)A"
        foreach ($log in $recentLogs) {
            Write-Host -NoNewLine "`r$($e)[K$log`n" -ForegroundColor Gray
        }

        for ($i = $recentLogs.Count + 1; $i -le $logLines; $i++) {
            Write-Host -NoNewLine "`r$($e)[K`n"
        }
    }

    Start-Sleep -Milliseconds 100
}

$jobs | Remove-Job

Write-Host "`rAll repositories pulled." -ForegroundColor Green