@echo off

echo "Usage: %~nx0 <lazarus_dir> <inno_setup_dir>"

if "%1" NEQ "" (
    set "LAZARUS_DIR=%1"
) else if not defined LAZARUS_DIR (
    set "LAZARUS_DIR=C:\lazarus"
)

if "%2" NEQ "" (
    set "ISC=%2"
) else if not defined ISC (
    set "ISC=C:\Program Files (x86)\Inno Setup 5"
)

set path=%LAZARUS_DIR%;%LAZARUS_DIR%\fpc\3.2.2\bin\i386-win32;%path%
set "PROG_VER="
set /p "PROG_VER="<..\..\VERSION.txt
if not defined PROG_VER goto err

if defined LAZARUS_PCP (
    lazbuild -B ../../trcomp.lpk "--lazarusdir=%LAZARUS_DIR%" "--pcp=%LAZARUS_PCP%"
    if errorlevel 1 goto err
    lazbuild -B ../../transgui.lpi "--lazarusdir=%LAZARUS_DIR%" "--pcp=%LAZARUS_PCP%"
) else (
    lazbuild -B ../../transgui.lpi
)
if errorlevel 1 goto err
make "PROG_VER=%PROG_VER%" -C ../.. clean
if errorlevel 1 goto err
make "PROG_VER=%PROG_VER%" -C ../.. all
if errorlevel 1 goto err

if not (%CODECERT%) == () (
  signtool.exe sign /d "Transmission Remote GUI" /du "https://github.com/transmission-remote-gui/transgui" /f "%CODECERT%" /fd sha256 /tr "http://timestamp.digicert.com" /td sha256 /v ..\..\transgui.exe
  if errorlevel 1 goto err
)

"%ISC%\iscc.exe" "/DAppVersion=%PROG_VER%" "/ssigntool=signtool.exe $p" setup.iss
if errorlevel 1 goto err

if not defined CI pause
exit /b 0

:err
if not defined CI pause
exit /b 1
