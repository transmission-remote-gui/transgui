#!/bin/sh
prog_ver=`cat ../../VERSION.txt`
build=`git rev-list --abbrev-commit --max-count=1 HEAD ../..`
exename=../../transgui
appname="Transmission Remote GUI"
dmgfolder=./Release
appfolder=$dmgfolder/$appname.app
lazdir=$1

#./compilers.sh

if [ ! "$lazdir" = "" ]
then
  lazdir=LAZARUS_DIR=$lazdir
fi

sed -i "s/'Version %s'/'Version %s Build $build'/" ../../about.lfm

# Building Intel version
make -C ../.. clean CPU_TARGET=i386 $lazdir
make -C ../.. CPU_TARGET=i386 $lazdir

if ! [ -e $exename ]
then
  echo "$exename does not exist"
  exit 1
fi
strip $exename

if [ -e "$appfolder" ]
then
  rm -r "$appfolder"
fi

echo "Creating $appfolder..."
mkdir -p "$appfolder"
mkdir "$appfolder/Contents"
mkdir "$appfolder/Contents/MacOS"
mkdir "$appfolder/Contents/Resources"

mv $exename "$appfolder/Contents/MacOS"
mkdir "$appfolder/Contents/MacOS/lang"
cp ../../lang/transgui.* "$appfolder/Contents/MacOS/lang"

cp ../../history.txt $dmgfolder
cp ../../readme.txt $dmgfolder

cp PkgInfo "$appfolder/Contents"
cp transgui.icns "$appfolder/Contents/Resources"
cat Info.plist | sed -e "s/@prog_ver@/$prog_ver/" > "$appfolder/Contents/Info.plist"

hdiutil create -ov -anyowners -volname transgui-$prog_ver -imagekey zlib-level=9 -format UDZO -srcfolder ./Release transgui-$prog_ver.dmg

rm -r "$dmgfolder"
#open transgui-$prog_ver.dmg