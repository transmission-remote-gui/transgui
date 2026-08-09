[CmdletBinding()]
param(
  [Parameter(Mandatory)]
  [ValidateSet('x86', 'x86_64')]
  [string]$Architecture,

  [string]$LazarusDir = 'C:\lazarus',

  [string]$InnoSetupDir = 'C:\Program Files (x86)\Inno Setup 5'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

function Invoke-CheckedCommand {
  param(
    [Parameter(Mandatory)]
    [string]$FilePath,

    [string[]]$ArgumentList = @()
  )

  Write-Host "> $FilePath $($ArgumentList -join ' ')"
  & $FilePath @ArgumentList
  if ($LASTEXITCODE -ne 0) {
    throw "Command failed with exit code ${LASTEXITCODE}: $FilePath"
  }
}

function Invoke-CheckedProcess {
  param(
    [Parameter(Mandatory)]
    [string]$FilePath,

    [string[]]$ArgumentList = @()
  )

  Write-Host "> $FilePath $($ArgumentList -join ' ')"
  $process = Start-Process `
    -FilePath $FilePath `
    -ArgumentList $ArgumentList `
    -PassThru `
    -Wait
  if ($process.ExitCode -ne 0) {
    throw "Process failed with exit code $($process.ExitCode): $FilePath"
  }
}

function Invoke-PackagingScript {
  param(
    [Parameter(Mandatory)]
    [string]$FilePath
  )

  Push-Location (Split-Path -Parent $FilePath)
  try {
    Invoke-CheckedCommand $FilePath
  }
  finally {
    Pop-Location
  }
}

function Assert-Artifact {
  param(
    [Parameter(Mandatory)]
    [string]$Path,

    [Parameter(Mandatory)]
    [version]$ExpectedVersion,

    [uint16]$ExpectedMachine = 0,

    [switch]$RequireUpx
  )

  if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
    throw "Expected file was not produced: $Path"
  }

  $versionInfo = [System.Diagnostics.FileVersionInfo]::GetVersionInfo($Path)
  $actualVersion = [version]::new(
    $versionInfo.FileMajorPart,
    $versionInfo.FileMinorPart,
    $versionInfo.FileBuildPart,
    $versionInfo.FilePrivatePart
  )
  $normalizedVersion = [version]::new(
    $ExpectedVersion.Major,
    $ExpectedVersion.Minor,
    [Math]::Max($ExpectedVersion.Build, 0),
    [Math]::Max($ExpectedVersion.Revision, 0)
  )
  if ($actualVersion -ne $normalizedVersion) {
    throw "$Path has file version $actualVersion, expected $normalizedVersion"
  }

  $signature = Get-AuthenticodeSignature -LiteralPath $Path
  if ($signature.Status -ne [System.Management.Automation.SignatureStatus]::NotSigned) {
    throw "$Path unexpectedly has Authenticode status $($signature.Status)"
  }

  if ($ExpectedMachine -ne 0) {
    $bytes = [System.IO.File]::ReadAllBytes($Path)
    if (
      $bytes.Length -lt 64 -or
      [BitConverter]::ToUInt16($bytes, 0) -ne 0x5A4D
    ) {
      throw "$Path does not have a valid DOS header"
    }

    $peOffset = [BitConverter]::ToInt32($bytes, 0x3C)
    if (
      $peOffset -lt 0 -or
      $peOffset + 6 -gt $bytes.Length -or
      [BitConverter]::ToUInt32($bytes, $peOffset) -ne 0x00004550
    ) {
      throw "$Path does not have a valid PE header"
    }

    $actualMachine = [BitConverter]::ToUInt16($bytes, $peOffset + 4)
    if ($actualMachine -ne $ExpectedMachine) {
      throw (
        '{0} has PE machine type 0x{1:X4}, expected 0x{2:X4}' -f
        $Path,
        $actualMachine,
        $ExpectedMachine
      )
    }
  }

  if ($RequireUpx) {
    Invoke-CheckedCommand $script:upx @('-t', $Path)
  }
}

function Assert-PathRemoved {
  param(
    [Parameter(Mandatory)]
    [string]$Path
  )

  foreach ($attempt in 1..40) {
    if (-not (Test-Path -LiteralPath $Path)) {
      return
    }
    Start-Sleep -Milliseconds 250
  }
  throw "Path was not removed: $Path"
}

function Restore-EnvironmentVariable {
  param(
    [Parameter(Mandatory)]
    [string]$Name,

    [AllowNull()]
    [string]$Value
  )

  if ($null -eq $Value) {
    Remove-Item "Env:$Name" -ErrorAction SilentlyContinue
  }
  else {
    Set-Item -Path "Env:$Name" -Value $Value
  }
}

$repositoryRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$versionText = (
  Get-Content -LiteralPath (Join-Path $repositoryRoot 'VERSION.txt') -TotalCount 1
).Trim()
if ([string]::IsNullOrWhiteSpace($versionText)) {
  throw 'VERSION.txt did not contain a version'
}
$version = [version]::Parse($versionText)

switch ($Architecture) {
  'x86' {
    $fpcTarget = 'i386-win32'
    $setupDirectory = 'setup\win'
    $expectedMachine = [uint16]0x014C
    $installerName = "transgui-$versionText-setup.exe"
  }
  'x86_64' {
    $fpcTarget = 'x86_64-win64'
    $setupDirectory = 'setup\win_amd64'
    $expectedMachine = [uint16]0x8664
    $installerName = "transgui-$versionText-setup_64bit.exe"
  }
}

$lazbuild = Join-Path $LazarusDir 'lazbuild.exe'
$setupPath = Join-Path $repositoryRoot $setupDirectory
$makeSetupScript = Join-Path $setupPath 'make_setup.bat'
$makeZipdistScript = Join-Path $setupPath 'make_zipdist.bat'
$script:upx = (Get-Command 'upx.exe' -CommandType Application -ErrorAction Stop).Source
$zip = (Get-Command 'zip.exe' -CommandType Application -ErrorAction Stop).Source

foreach ($requiredFile in @(
  $lazbuild,
  (Join-Path $InnoSetupDir 'ISCC.exe'),
  $script:upx,
  $zip,
  $makeSetupScript,
  $makeZipdistScript
)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required build tool or script was not found: $requiredFile"
  }
}

$originalEnvironment = [ordered]@{
  CI = $env:CI
  CODECERT = $env:CODECERT
  ISC = $env:ISC
  LAZARUS_DIR = $env:LAZARUS_DIR
  LAZARUS_PCP = $env:LAZARUS_PCP
}

$tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { $env:TEMP }
$primaryConfigPath = Join-Path $tempRoot "lazarus-pcp-transgui-$Architecture"
$portableTestPath = Join-Path $tempRoot "transgui-portable-$Architecture"
$installerTestPath = Join-Path $tempRoot "transgui-installer-$Architecture"
$installerLog = Join-Path $tempRoot "transgui-installer-$Architecture.log"

$env:CI = 'true'
$env:ISC = $InnoSetupDir
$env:LAZARUS_DIR = $LazarusDir
$env:LAZARUS_PCP = $primaryConfigPath
Remove-Item Env:CODECERT -ErrorAction SilentlyContinue

foreach ($path in @(
  $primaryConfigPath,
  $portableTestPath,
  $installerTestPath,
  (Join-Path $repositoryRoot 'units'),
  (Join-Path $repositoryRoot 'lib'),
  (Join-Path $repositoryRoot 'Release'),
  (Join-Path $repositoryRoot 'transgui.exe')
)) {
  Remove-Item -LiteralPath $path -Recurse -Force -ErrorAction SilentlyContinue
}
Remove-Item -LiteralPath $installerLog -Force -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Path $primaryConfigPath | Out-Null

try {
  Invoke-PackagingScript $makeSetupScript

  $releaseDirectory = Join-Path $repositoryRoot 'Release'
  $installer = Join-Path $releaseDirectory $installerName
  Assert-Artifact $installer $version

  Invoke-CheckedProcess $installer @(
    '/VERYSILENT',
    '/SUPPRESSMSGBOXES',
    '/NORESTART',
    '/SP-',
    '/TYPE=compact',
    '/TASKS=',
    ('/DIR="{0}"' -f $installerTestPath),
    ('/LOG="{0}"' -f $installerLog)
  )

  $installedExecutable = Join-Path $installerTestPath 'transgui.exe'
  Assert-Artifact $installedExecutable $version $expectedMachine

  $uninstallers = @(
    Get-ChildItem -LiteralPath $installerTestPath -Filter 'unins*.exe' -File
  )
  if ($uninstallers.Count -ne 1) {
    throw "Expected exactly one uninstaller; found $($uninstallers.Count)"
  }
  Invoke-CheckedProcess $uninstallers[0].FullName @(
    '/VERYSILENT',
    '/SUPPRESSMSGBOXES',
    '/NORESTART'
  )
  Assert-PathRemoved $installedExecutable

  Invoke-PackagingScript $makeZipdistScript

  $portableArchive = Join-Path (
    Join-Path $repositoryRoot 'Release'
  ) "transgui-$versionText-$fpcTarget.zip"
  if (-not (Test-Path -LiteralPath $portableArchive -PathType Leaf)) {
    throw "Expected portable archive was not produced: $portableArchive"
  }

  Expand-Archive -LiteralPath $portableArchive -DestinationPath $portableTestPath
  Assert-Artifact `
    (Join-Path $portableTestPath 'transgui.exe') `
    $version `
    $expectedMachine `
    -RequireUpx

  Get-Item -LiteralPath $installer, $portableArchive |
    Format-Table FullName, Length, LastWriteTimeUtc -AutoSize
}
catch {
  if (Test-Path -LiteralPath $installerLog -PathType Leaf) {
    Write-Host '--- Inno Setup log ---'
    Get-Content -LiteralPath $installerLog | ForEach-Object { Write-Host $_ }
  }
  throw
}
finally {
  foreach ($path in @(
    $primaryConfigPath,
    $portableTestPath,
    $installerTestPath
  )) {
    Remove-Item -LiteralPath $path -Recurse -Force -ErrorAction SilentlyContinue
  }
  Remove-Item -LiteralPath $installerLog -Force -ErrorAction SilentlyContinue

  foreach ($entry in $originalEnvironment.GetEnumerator()) {
    Restore-EnvironmentVariable $entry.Key $entry.Value
  }
}
