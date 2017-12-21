@echo off

if (%1) == () goto usage
if (%2) == () goto usage

set path=%1;%1\fpc\3.0.2\bin\i386-win32;%path%
set LAZARUS_DIR=%1

lazbuild -B ../../transgui.lpi
make -C ../.. clean LAZARUS_DIR=%1
make -C ../.. all   LAZARUS_DIR=%1
if errorlevel 1 goto err

if not (%CODECERT%) == () (
  signtool.exe sign /d "Transmission Remote GUI" /du "https://github.com/transmission-remote-gui/transgui" /f "%CODECERT%" /v ..\..\transgui.exe
  if errorlevel 1 goto err
)

set ISC=%~2

"%ISC%\iscc.exe" "/ssigntool=signtool.exe $p" setup.iss
if errorlevel 1 goto err

exit /b 0

:usage
echo "Usage: %~nx0 <lazarus_dir> <inno_setup_dir>"

:err
pause
exit /b 1
