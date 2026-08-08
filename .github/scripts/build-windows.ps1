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

function Invoke-ExternalCommand {
  param(
    [Parameter(Mandatory)]
    [string]$FilePath,

    [Parameter()]
    [string[]]$ArgumentList = @()
  )

  Write-Host "> $FilePath $($ArgumentList -join ' ')"
  & $FilePath @ArgumentList

  if ($LASTEXITCODE -ne 0) {
    throw "Command failed with exit code ${LASTEXITCODE}: $FilePath"
  }
}

function Invoke-WaitProcess {
  param(
    [Parameter(Mandatory)]
    [string]$FilePath,

    [Parameter()]
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
    [string]$FilePath,

    [Parameter()]
    [string[]]$ArgumentList = @()
  )

  Push-Location (Split-Path -Parent $FilePath)
  try {
    Invoke-ExternalCommand -FilePath $FilePath -ArgumentList $ArgumentList
  }
  finally {
    Pop-Location
  }
}

function Assert-PeMachine {
  param(
    [Parameter(Mandatory)]
    [string]$Path,

    [Parameter(Mandatory)]
    [uint16]$ExpectedMachine
  )

  $stream = [System.IO.File]::Open(
    $Path,
    [System.IO.FileMode]::Open,
    [System.IO.FileAccess]::Read,
    [System.IO.FileShare]::Read
  )
  $reader = [System.IO.BinaryReader]::new($stream)

  try {
    if ($reader.ReadUInt16() -ne 0x5A4D) {
      throw "$Path does not have a valid DOS header"
    }

    $stream.Position = 0x3C
    $peOffset = $reader.ReadInt32()
    $stream.Position = $peOffset

    if ($reader.ReadUInt32() -ne 0x00004550) {
      throw "$Path does not have a valid PE header"
    }

    $actualMachine = $reader.ReadUInt16()
    if ($actualMachine -ne $ExpectedMachine) {
      throw (
        '{0} has PE machine type 0x{1:X4}, expected 0x{2:X4}' -f
        $Path,
        $actualMachine,
        $ExpectedMachine
      )
    }
  }
  finally {
    $reader.Dispose()
    $stream.Dispose()
  }
}

function Assert-FileVersion {
  param(
    [Parameter(Mandatory)]
    [string]$Path,

    [Parameter(Mandatory)]
    [string]$ExpectedVersion
  )

  $expected = [System.Version]::Parse($ExpectedVersion)
  $expectedParts = @(
    $expected.Major,
    $expected.Minor,
    [Math]::Max($expected.Build, 0),
    [Math]::Max($expected.Revision, 0)
  )

  $versionInfo = [System.Diagnostics.FileVersionInfo]::GetVersionInfo($Path)
  $actualParts = @(
    $versionInfo.FileMajorPart,
    $versionInfo.FileMinorPart,
    $versionInfo.FileBuildPart,
    $versionInfo.FilePrivatePart
  )

  $expectedText = $expectedParts -join '.'
  $actualText = $actualParts -join '.'
  if ($actualText -ne $expectedText) {
    throw "$Path has file version $actualText, expected $expectedText"
  }
}

function Assert-Unsigned {
  param(
    [Parameter(Mandatory)]
    [string]$Path
  )

  $signature = Get-AuthenticodeSignature -LiteralPath $Path
  if ($signature.Status -ne [System.Management.Automation.SignatureStatus]::NotSigned) {
    throw "$Path unexpectedly has Authenticode status $($signature.Status)"
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
$version = (
  Get-Content -LiteralPath (Join-Path $repositoryRoot 'VERSION.txt') -TotalCount 1
).Trim()
if ([string]::IsNullOrWhiteSpace($version)) {
  throw 'VERSION.txt did not contain a version'
}

switch ($Architecture) {
  'x86' {
    $fpcTarget = 'i386-win32'
    $setupDirectory = 'setup\win'
    $expectedMachine = [uint16]0x014C
    $installerName = "transgui-$version-setup.exe"
  }
  'x86_64' {
    $fpcTarget = 'x86_64-win64'
    $setupDirectory = 'setup\win_amd64'
    $expectedMachine = [uint16]0x8664
    $installerName = "transgui-$version-setup_64bit.exe"
  }
}

$lazbuild = Join-Path $LazarusDir 'lazbuild.exe'
$fpcRoot = Join-Path $LazarusDir 'fpc'
$fpcCandidates = @(
  Get-ChildItem -LiteralPath $fpcRoot -Directory |
    Where-Object {
      Test-Path -LiteralPath (
        Join-Path $_.FullName "bin\$fpcTarget\make.exe"
      )
    }
)
if ($fpcCandidates.Count -ne 1) {
  $candidatePaths = $fpcCandidates.FullName -join ', '
  throw (
    "Expected exactly one FPC installation for $fpcTarget; " +
    "found $($fpcCandidates.Count): $candidatePaths"
  )
}

$fpcBin = Join-Path $fpcCandidates[0].FullName "bin\$fpcTarget"
$make = Join-Path $fpcBin 'make.exe'
$iscc = Join-Path $InnoSetupDir 'ISCC.exe'
$setupPath = Join-Path $repositoryRoot $setupDirectory
$makeSetupScript = Join-Path $setupPath 'make_setup.bat'
$makeZipdistScript = Join-Path $setupPath 'make_zipdist.bat'

$zipCommand = Get-Command 'zip.exe' -CommandType Application -ErrorAction Stop
$upxCommand = Get-Command 'upx.exe' -CommandType Application -ErrorAction Stop
$zip = $zipCommand.Source
$upx = $upxCommand.Source

foreach ($requiredFile in @(
  $lazbuild,
  $make,
  $iscc,
  $zip,
  $upx,
  $makeSetupScript,
  $makeZipdistScript
)) {
  if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
    throw "Required build tool or script was not found: $requiredFile"
  }
}

$originalPath = $env:PATH
$originalCi = $env:CI
$originalCodeCert = $env:CODECERT
$originalIsc = $env:ISC
$originalLazarusDir = $env:LAZARUS_DIR
$originalLazarusPcp = $env:LAZARUS_PCP

$env:PATH = "$LazarusDir;$fpcBin;$env:PATH"
$env:CI = 'true'
$env:ISC = $InnoSetupDir
$env:LAZARUS_DIR = $LazarusDir
Remove-Item Env:CODECERT -ErrorAction SilentlyContinue

$tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { $env:TEMP }
$primaryConfigPath = Join-Path $tempRoot "lazarus-pcp-transgui-$Architecture"
$portableTestPath = Join-Path $tempRoot "transgui-portable-$Architecture"
$installerTestPath = Join-Path $tempRoot "transgui-installer-$Architecture"
$installerLog = Join-Path $tempRoot "transgui-installer-$Architecture.log"
$env:LAZARUS_PCP = $primaryConfigPath

foreach ($temporaryPath in @(
  $primaryConfigPath,
  $portableTestPath,
  $installerTestPath
)) {
  Remove-Item -LiteralPath $temporaryPath -Recurse -Force -ErrorAction SilentlyContinue
}
Remove-Item -LiteralPath $installerLog -Force -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Path $primaryConfigPath | Out-Null

$generatedPaths = @(
  (Join-Path $repositoryRoot 'units'),
  (Join-Path $repositoryRoot 'lib'),
  (Join-Path $repositoryRoot 'Release'),
  (Join-Path $repositoryRoot 'transgui.exe')
)
Remove-Item -LiteralPath $generatedPaths -Recurse -Force -ErrorAction SilentlyContinue

try {
  Invoke-PackagingScript -FilePath $makeSetupScript

  $executable = Join-Path $repositoryRoot 'transgui.exe'
  if (-not (Test-Path -LiteralPath $executable -PathType Leaf)) {
    throw "Windows executable was not produced: $executable"
  }
  Assert-PeMachine -Path $executable -ExpectedMachine $expectedMachine
  Assert-FileVersion -Path $executable -ExpectedVersion $version
  Assert-Unsigned -Path $executable

  $releaseDirectory = Join-Path $repositoryRoot 'Release'
  $installer = Join-Path $releaseDirectory $installerName
  if (-not (Test-Path -LiteralPath $installer -PathType Leaf)) {
    throw "Expected installer was not produced: $installer"
  }
  Assert-FileVersion -Path $installer -ExpectedVersion $version
  Assert-Unsigned -Path $installer

  Invoke-WaitProcess $installer @(
    '/VERYSILENT',
    '/SUPPRESSMSGBOXES',
    '/NORESTART',
    '/SP-',
    '/TYPE=compact',
    '/TASKS=',
    ('/DIR="{0}"' -f $installerTestPath),
    ('/LOG="{0}"' -f $installerLog)
  )

  $installedExecutables = @(
    Get-ChildItem `
      -LiteralPath $installerTestPath `
      -Filter 'transgui.exe' `
      -File `
      -Recurse
  )
  if ($installedExecutables.Count -ne 1) {
    throw (
      'Expected exactly one installed transgui.exe; ' +
      "found $($installedExecutables.Count)"
    )
  }
  $installedExecutable = $installedExecutables[0].FullName
  Assert-PeMachine -Path $installedExecutable -ExpectedMachine $expectedMachine
  Assert-FileVersion -Path $installedExecutable -ExpectedVersion $version
  Assert-Unsigned -Path $installedExecutable

  $uninstallers = @(
    Get-ChildItem `
      -LiteralPath $installerTestPath `
      -Filter 'unins*.exe' `
      -File `
      -Recurse
  )
  if ($uninstallers.Count -ne 1) {
    throw "Expected exactly one uninstaller; found $($uninstallers.Count)"
  }
  Invoke-WaitProcess $uninstallers[0].FullName @(
    '/VERYSILENT',
    '/SUPPRESSMSGBOXES',
    '/NORESTART'
  )
  Assert-PathRemoved -Path $installedExecutable

  Invoke-PackagingScript -FilePath $makeZipdistScript

  if (-not (Test-Path -LiteralPath $executable -PathType Leaf)) {
    throw "Portable build did not produce transgui.exe: $executable"
  }
  Assert-PeMachine -Path $executable -ExpectedMachine $expectedMachine
  Assert-FileVersion -Path $executable -ExpectedVersion $version
  Assert-Unsigned -Path $executable
  Invoke-ExternalCommand $upx @('-t', $executable)

  $portableArchive = Join-Path (
    Join-Path $repositoryRoot 'Release'
  ) "transgui-$version-$fpcTarget.zip"
  if (-not (Test-Path -LiteralPath $portableArchive -PathType Leaf)) {
    throw "Expected portable archive was not produced: $portableArchive"
  }

  Expand-Archive -LiteralPath $portableArchive -DestinationPath $portableTestPath
  $portableExecutables = @(
    Get-ChildItem `
      -LiteralPath $portableTestPath `
      -Filter 'transgui.exe' `
      -File `
      -Recurse
  )
  if ($portableExecutables.Count -ne 1) {
    throw (
      'Expected exactly one transgui.exe in the portable archive; ' +
      "found $($portableExecutables.Count)"
    )
  }
  $portableExecutable = $portableExecutables[0].FullName
  Assert-PeMachine -Path $portableExecutable -ExpectedMachine $expectedMachine
  Assert-FileVersion -Path $portableExecutable -ExpectedVersion $version
  Assert-Unsigned -Path $portableExecutable
  Invoke-ExternalCommand $upx @('-t', $portableExecutable)

  foreach ($artifact in @(
    (Get-Item -LiteralPath $portableArchive),
    (Get-Item -LiteralPath $installer)
  )) {
    if ($artifact.Length -le 0) {
      throw "Artifact is empty: $($artifact.FullName)"
    }
    $artifact | Format-List FullName, Length, LastWriteTimeUtc
  }
}
catch {
  if (Test-Path -LiteralPath $installerLog -PathType Leaf) {
    Write-Host '--- Inno Setup log ---'
    Get-Content -LiteralPath $installerLog | ForEach-Object { Write-Host $_ }
  }
  throw
}
finally {
  foreach ($temporaryPath in @(
    $primaryConfigPath,
    $portableTestPath,
    $installerTestPath
  )) {
    Remove-Item -LiteralPath $temporaryPath -Recurse -Force -ErrorAction SilentlyContinue
  }
  Remove-Item -LiteralPath $installerLog -Force -ErrorAction SilentlyContinue

  Restore-EnvironmentVariable -Name 'PATH' -Value $originalPath
  Restore-EnvironmentVariable -Name 'CI' -Value $originalCi
  Restore-EnvironmentVariable -Name 'CODECERT' -Value $originalCodeCert
  Restore-EnvironmentVariable -Name 'ISC' -Value $originalIsc
  Restore-EnvironmentVariable -Name 'LAZARUS_DIR' -Value $originalLazarusDir
  Restore-EnvironmentVariable -Name 'LAZARUS_PCP' -Value $originalLazarusPcp
}
