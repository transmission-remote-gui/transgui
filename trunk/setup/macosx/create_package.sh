#!/bin/sh

pm="/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker"
proot=../../proot
progver=1.4

echo Preparing package contents...

if [ -e "$proot" ]
then
  rm -r "$proot"
fi

mkdir $proot
mkdir $proot/Applications
#mv "../../Transmission Remote GUI.app" $proot/Applications
cp -R "../../Transmission Remote GUI.app" $proot/Applications

# fix permissions
# everyone can read, group can write
find $proot -exec chmod a+r,g+w {} \;
# what is executable should be executable by everyone
find $proot -perm +o+x -exec chmod a+x {} \;
# everyone can access directories
find $proot -type d -exec chmod a+x {} \;

mkdir ./Resources
cp ../../LICENSE.txt ./Resources/License.txt

echo Creating package...

mkdir ./image
$pm --root $proot --id com.transgui --version $progver --title "Transmission Remote GUI $progver" --target 10.4 --resources ./Resources --out ./image/transgui.pkg
rm -r ./Resources
rm -r "$proot"

echo Creating disk image...
hdiutil create -ov -anyowners -volname transgui-$progver -imagekey zlib-level=9 -format UDZO -srcfolder ./image transgui-$progver.dmg

rm -r ./image
