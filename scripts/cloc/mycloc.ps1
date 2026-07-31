Param(
    [Parameter(Mandatory = $true)]$directory,
    $img = ""
)

$abspath = Convert-Path $directory
$imgpath = ""
if ($img -ne "") {
    New-Item -Path $img -ItemType "File"
    $imgpath = Convert-Path $img
}

$clocPath = Join-Path $PSScriptRoot "cloc"

Push-Location $directory

$isgit = git status
if (-not $isgit) {
    Write-Error "git repositoryではありません: $directory"
}

Pop-Location

Push-Location $clocPath

Write-Host "Searching $abspath" -ForegroundColor Green

$tmpfile = New-TemporaryFile
$outfile = New-TemporaryFile

./cloc.exe $abspath $tmpfile.FullName
node ./main.js $tmpfile.FullName $outfile.FullName

if ($imgpath -ne "") {
    Copy-Item -Path $outfile.FullName -Destination $imgpath
}

Remove-Item $tmpfile
Remove-Item $outfile

Pop-Location