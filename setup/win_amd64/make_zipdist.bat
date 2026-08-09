@echo off

echo "Usage: %~nx0 <lazarus_dir>"

if "%1" NEQ "" (
    set "LAZARUS_DIR=%1"
) else if not defined LAZARUS_DIR (
    set "LAZARUS_DIR=C:\lazarus"
)

set path=%LAZARUS_DIR%;%LAZARUS_DIR%\fpc\3.2.2\bin\x86_64-win64;%path%
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
upx --best ../../transgui.exe
if errorlevel 1 goto err
make "PROG_VER=%PROG_VER%" -C ../.. zipdist
if errorlevel 1 goto err

if not defined CI pause
exit /b 0

:err
if not defined CI pause
exit /b 1
