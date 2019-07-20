@echo off

net session >nul 2>&1

if %errorLevel% NEQ 0 (
    echo Need Administrator permission to install dependencies
    pause
    exit
)

where choco >nul 2>&1

if %errorLevel% == 0 (
    echo Found Chocolatey
    choco --version
) else (
    echo Install Chocolatey...
    @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"
)

if exist "C:\Program Files (x86)\Inno Setup 5\ISCC.exe" (
    echo Found Inno Setup v5
) else (
    choco install innosetup --version=5.6.1 -y
)

if exist "C:\Program Files (x86)\Inno Download Plugin\idp.iss" (
    echo Found Inno Download Plugin
) else (
    choco install inno-download-plugin -y
)

if exist "C:\lazarus\lazbuild.exe" (
    echo Found Lazarus
) else (
    choco install lazarus --x86 -y
)

if exist "C:\Program Files\Git\git-cmd.exe" (
    echo Found git
) else (
    choco install git -y
)

pause
