#!/bin/sh

prog_ver=`cat ../../VERSION.txt`
exename=../../transgui
appname="Transmission Remote GUI"
appfolder=../../$appname.app
lazdir=$1

if [ "$lazdir" = "" ]
then
  echo "Lazarus dir was not specified."
  exit 1
fi

# Building Intel version
make -C ../.. clean LAZARUS_DIR=$lazdir CPU_TARGET=i386
make -C ../.. LAZARUS_DIR=$lazdir CPU_TARGET=i386
strip $exename
mv $exename $exename.386

# Building PowerPC version
make -C ../.. clean LAZARUS_DIR=$lazdir CPU_TARGET=powerpc
make -C ../.. LAZARUS_DIR=$lazdir CPU_TARGET=powerpc
strip $exename
mv $exename $exename.ppc

# Creating universal executable
lipo -create $exename.ppc $exename.386 -output $exename
rm $exename.386
rm $exename.ppc

if ! [ -e $exename ]
then
  echo "$exename does not exist"
  exit 1
fi
if [ -e "$appfolder" ]
then
  rm -r "$appfolder"
fi

echo "Creating $appfolder..."
mkdir "$appfolder"
mkdir "$appfolder/Contents"
mkdir "$appfolder/Contents/MacOS"
mkdir "$appfolder/Contents/Resources"

mv $exename "$appfolder/Contents/MacOS"
cp PkgInfo "$appfolder/Contents"
cp transgui.icns "$appfolder/Contents/Resources"
cat Info.plist | sed -e "s/@prog_ver@/$prog_ver/" > "$appfolder/Contents/Info.plist"
