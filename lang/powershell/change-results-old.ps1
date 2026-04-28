$e = [char]27
$tasks = @(
    "TaskA",
    "TaskB",
    "TaskC"
)

foreach ($t in $tasks) {
    Write-Host "$($t): Starting..."
}

$jobs = foreach ($t in $tasks) {
    Start-Job -ScriptBlock {
        param($name)

        Start-Sleep -Seconds (Get-Random -Minimum 1 -Maximum 5)
        return $name
    } -ArgumentList $t
}

$completedTasks = @()
while ($completedTasks.Count -lt $tasks.Count) {
    foreach ($job in $jobs) {
        if ($job.State -eq "Completed" -and $job.Name -notin $completedTasks) {
            $t = Receive-Job -Job $job
            $completedTasks += $job.Name

            $index = [array]::IndexOf($tasks, $t)
            $upCount = $tasks.Count - $index

            Write-Host -NoNewline "$($e)[$($upCount)A`r$($t): Completed."
            Write-Host -NoNewline "$($e)[$($upCount)B"
        }
    }

    Start-Sleep -Milliseconds 100
}

$jobs | Remove-Job

Write-Host "`rAll tasks completed."