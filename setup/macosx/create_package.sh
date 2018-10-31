#!/bin/sh

prog_ver="$(cat ../../VERSION.txt)"
pm="/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker"
proot=../../proot

echo Preparing package contents...

rm -rf "$proot"

mkdir -p "$proot/Applications"
cp -R "../../Transmission Remote GUI.app" "$proot/Applications"

# fix permissions
# everyone can read, group can write
find "$proot" -exec chmod a+r,g+w {} \;
# what is executable should be executable by everyone
find "$proot" -perm +o+x -exec chmod a+x {} \;
# everyone can access directories
find "$proot" -type d -exec chmod a+x {} \;

mkdir ./Resources
cp ../../LICENSE ./Resources/License.txt

echo Creating package...

mkdir ./image
"$pm" --root "$proot" --info ./package.plist --version "$prog_ver" --title "Transmission Remote GUI $prog_ver" --resources ./Resources --target 10.4 --no-relocate --out ./image/transgui.pkg
rm -r ./Resources
rm -r "$proot"

cp ../../history.txt ./image
cp ../../README.md ./image

echo Creating disk image...
mkdir -p "../../Release"

hdiutil create -ov -anyowners -volname "transgui-$prog_ver" -imagekey zlib-level=9 -format UDBZ -srcfolder ./image "../../Release/transgui-$prog_ver.dmg"

rm -r ./image
