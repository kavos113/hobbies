Param(
    $Dirpath,
    $Verbose
)

class FileInfo {
    [string]$FileName
    [int]$Lines

    FileInfo([string]$name, [int]$lines) {
        $this.FileName = $name
        $this.Lines = $lines
    }
}

$results = Get-ChildItem $Dirpath -File | ForEach-Object -Process {
    $l = (Get-Content $_.FullName | Measure-Object).Count
    return [FileInfo]::new($_.Name, $l)
}

if ($Verbose -eq "true") {
    $results | Format-Table -AutoSize
}

$total = ($results | Measure-Object -Property Lines -Sum).Sum
Write-Host "$Dirpath : $total lines"