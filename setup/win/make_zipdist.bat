@echo off

echo "Usage: %~nx0 <lazarus_dir>"

if "%1" NEQ "" (
    set "LAZARUS_DIR=%1"
) else (
    set "LAZARUS_DIR=C:\lazarus"
)

set path=%LAZARUS_DIR%;%LAZARUS_DIR%\fpc\3.2.2\bin\i386-win32;%path%

lazbuild -B ../../transgui.lpi
if errorlevel 1 goto err
make -C ../.. clean
if errorlevel 1 goto err
make -C ../.. all
if errorlevel 1 goto err
upx --best ../../transgui.exe
if errorlevel 1 goto err
make -C ../.. zipdist
if errorlevel 1 goto err

pause
exit /b 0

:err
pause
exit /b 1
