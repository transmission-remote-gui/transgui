#!/bin/sh

set -x

prog_ver="$(cat ../../VERSION.txt)"
build="$(git rev-list --abbrev-commit --max-count=1 HEAD ../..)"
exename=../../transgui
appname="Transmission Remote GUI"
dmg_dist_file="../../Release/transgui-$prog_ver.dmg"
dmgfolder=./Release
appfolder="$dmgfolder/$appname.app"

if [ -z "${CI-}" ]; then
  ./install_deps.sh
fi

# Detect Lazarus and FPC paths (Homebrew on Apple Silicon or Intel, fallback to legacy)
LAZDIR=$(brew --prefix lazarus 2>/dev/null || ls -d /Applications/Lazarus 2>/dev/null || echo "/Library/Lazarus")
LAZBUILD=$(find "$LAZDIR" /usr/local/bin -name "lazbuild" 2>/dev/null | head -1)
FPCBIN=$(which fpc)
lazdir="${1:-$LAZDIR}"

lazarus_ver="$("$LAZBUILD" -v 2>/dev/null || echo unknown)"
fpc_ver="$(fpc -i V | head -n 1)"

mkdir -p ../../Release/
sed -i.bak "s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver'/" ../../about.lfm

"$LAZBUILD" -B ../../transgui.lpi --lazarusdir="$lazdir" --compiler="$FPCBIN" --cpu=x86_64 --widgetset=cocoa

# Building x86_64 version (runs via Rosetta 2 on Apple Silicon)
make -j"$(sysctl -n hw.ncpu)" -C ../.. clean CPU_TARGET=x86_64 LAZARUS_DIR="$lazdir"
make -j"$(sysctl -n hw.ncpu)" -C ../.. CPU_TARGET=x86_64 LAZARUS_DIR="$lazdir"

if ! [ -e $exename ]; then
  echo "$exename does not exist"
  exit 1
fi
strip "$exename"

rm -rf "$appfolder"

echo "Creating $appfolder..."
mkdir -p "$appfolder/Contents/MacOS/lang"
mkdir -p "$appfolder/Contents/Resources"

mv "$exename" "$appfolder/Contents/MacOS"
cp ../../lang/transgui.* "$appfolder/Contents/MacOS/lang"

cp ../../history.txt "$dmgfolder"
cp ../../README.md "$dmgfolder"

cp PkgInfo "$appfolder/Contents"
cp transgui.icns "$appfolder/Contents/Resources"
sed -e "s/@prog_ver@/$prog_ver/" Info.plist > "$appfolder/Contents/Info.plist"

ln -s /Applications "$dmgfolder/Drag \"Transmission Remote GUI\" here!"

hdiutil create -ov -anyowners -volname "transgui-v$prog_ver" -format UDRW -srcfolder ./Release -fs HFS+ "tmp.dmg"

mount_device="$(hdiutil attach -readwrite -noautoopen "tmp.dmg" | awk 'NR==1{print$1}')"
mount_volume="$(mount | grep "$mount_device" | sed 's/^[^ ]* on //;s/ ([^)]*)$//')"

# SetFile was removed in Xcode 13+; skip volume icon if unavailable
if command -v SetFile >/dev/null 2>&1; then
  cp transgui.icns "$mount_volume/.VolumeIcon.icns"
  SetFile -c icnC "$mount_volume/.VolumeIcon.icns"
  SetFile -a C "$mount_volume"
fi

hdiutil detach "$mount_device"
rm -f "$dmg_dist_file"
hdiutil convert tmp.dmg -format UDBZ -imagekey zlib-level=9 -o "$dmg_dist_file"

rm tmp.dmg
rm -rf "$dmgfolder"
mv ../../about.lfm.bak ../../about.lfm

# Ad-hoc sign the app so macOS Sequoia honours Local Network permission
xattr -cr "$appfolder"
codesign --force --deep --sign - "$appfolder"

if [ -z "${CI-}" ]; then
  open "$dmg_dist_file"
fi
