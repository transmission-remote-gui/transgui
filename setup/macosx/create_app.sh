#!/bin/sh

prog_ver="$(cat ../../VERSION.txt)"
exename=../../transgui
appname="Transmission Remote GUI"
appfolder="../../$appname.app"
lazdir="${1:-/Developer/lazarus/}"

if [ ! "$lazdir" = "" ]
then
  lazdir=LAZARUS_DIR="$lazdir"
fi

# Building Intel version
make -C ../.. clean CPU_TARGET=i386 "$lazdir"
make -C ../.. CPU_TARGET=i386 "$lazdir"
strip "$exename"
mv "$exename" "$exename.386"

# Building PowerPC version
make -C ../.. clean CPU_TARGET=powerpc "$lazdir"
make -C ../.. CPU_TARGET=powerpc "$lazdir"
strip "$exename"
mv "$exename" "$exename.ppc"

# Creating universal executable
lipo -create "$exename.ppc" "$exename.386" -output "$exename"
rm "$exename.386" "$exename.ppc"

if ! [ -e $exename ]
then
  echo "$exename does not exist"
  exit 1
fi

rm -rf "$appfolder"

echo "Creating $appfolder..."
mkdir -p "$appfolder/Contents/MacOS"
mkdir -p "$appfolder/Contents/Resources"

mv "$exename" "$appfolder/Contents/MacOS"
mkdir "$appfolder/Contents/MacOS/lang"
cp ../../lang/transgui.* "$appfolder/Contents/MacOS/lang"

cp PkgInfo "$appfolder/Contents"
cp transgui.icns "$appfolder/Contents/Resources"
sed -e "s/@prog_ver@/$prog_ver/" Info.plist > "$appfolder/Contents/Info.plist"
