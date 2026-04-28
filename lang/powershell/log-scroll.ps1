$e = [char]27

$tasks = 1..50
$displayLines = 5
$currentLogs = New-Object System.Collections.Generic.List[string]

for ($i = 1; $i -le $displayLines; $i++) {
    Write-Host ""
}

foreach ($t in $tasks) {
    Start-Sleep -Milliseconds 300

    $msg = "[$(Get-Date -Format HH:mm:ss)] Task $t completed."
    $currentLogs.Add($msg)

    if ($currentLogs.Count -gt $displayLines) {
        $currentLogs.RemoveAt(0)
    }

    Write-Host -NoNewline "$($e)[$($displayLines)A"

    foreach ($log in $currentLogs) {
        Write-Host -NoNewline "`r$($e)[K$log`n"
    }
}

Write-Host "All tasks completed."