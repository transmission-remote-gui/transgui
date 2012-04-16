@echo off

if (%1) == () goto usage
if (%2) == () goto usage

make -C ../.. clean all LAZARUS_DIR="%1"
if errorlevel 1 goto err

if not (%CODECERT%) == () (
  signtool.exe sign /d "Transmission Remote GUI" /du "http://code.google.com/p/transmisson-remote-gui/" /f "%CODECERT%" /t "http://timestamp.verisign.com/scripts/timestamp.dll" /v ..\..\transgui.exe
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
