$e = [char]27
$tasks = @(
    "TaskA",
    "TaskB",
    "TaskC"
)
$lock = New-Object Object 

foreach ($t in $tasks) {
    Write-Host "$($t): Starting..."
}

$tasks | ForEach-Object -Parallel {
    $l = $using:lock
    $t = $_

    $allTasks = $using:tasks
    $esc = $using:e

    Start-Sleep -Seconds (Get-Random -Minimum 1 -Maximum 5)

    lock ($l) {
        $index = [array]::IndexOf($allTasks, $t)
        $upCount = $allTasks.Count - $index

        Write-Host -NoNewline "$($esc)[$($upCount)A`r$($t): Completed."
        Write-Host -NoNewline "$($esc)[$($upCount)B"
    }
} -ThrottleLimit $tasks.Count

Write-Host "All tasks completed."