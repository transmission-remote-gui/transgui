@echo off

if (%1) == () goto usage
if (%2) == () goto usage

make -C ../.. LAZARUS_DIR="%1"
if errorlevel 1 goto err

set ISC=%~2
)

"%ISC%\iscc.exe" setup.iss
if errorlevel 1 goto err

exit /b 0

:usage
echo "Usage: %~nx0 <lazarus_dir> <inno_setup_dir>"

:err
pause
exit /b 1
