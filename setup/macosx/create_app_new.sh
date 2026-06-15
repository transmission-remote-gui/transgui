#!/bin/sh

set -x

# Build a native macOS .app + .dmg for Transmission Remote GUI.
#
# Latest-Mac (Apple Silicon / macOS 26 Tahoe) notes:
#   * Default target is aarch64 (Apple Silicon). Override with CPU=x86_64.
#   * Requires a recent Lazarus/FPC that supports aarch64-darwin Cocoa. The
#     Lazarus 4.x "macOS aarch64" release builds work; for macOS 26 the Lazarus
#     development (main) LCL is recommended (fewer Cocoa layout issues).
#     Point LAZARUS_DIR / FPC at your install (see install_deps.sh).
#   * transgui.lpi pins optimization off (-O-): FPC 3.2.4's optimizer
#     miscompiles a virtual-method-call sequence on aarch64-darwin and corrupts
#     the Synapse socket object, crashing on connect. Do not re-enable -O here.
#   * The binary MUST be (re)code-signed after it is placed in the .app, or
#     macOS 26 SIGKILLs it ("Code Signature Invalid") when it faults in code
#     pages at runtime. We ad-hoc sign by default; set SIGN_ID to a real
#     "Developer ID Application: ..." identity to distribute.

prog_ver="$(cat ../../VERSION.txt)"
build="$(git rev-list --abbrev-commit --max-count=1 HEAD ../..)"
lazarus_ver="$(lazbuild -v 2>/dev/null)"
fpc_ver="$(fpc -i V 2>/dev/null | head -n 1)"
appname="Transmission Remote GUI"
dmg_dist_file="../../Release/transgui-$prog_ver.dmg"
dmgfolder=./Release
appfolder="$dmgfolder/$appname.app"

# Configurable toolchain / target (sane defaults for Apple Silicon).
CPU="${CPU:-aarch64}"
LAZARUS_DIR="${LAZARUS_DIR:-${1:-/Library/Lazarus/}}"
FPC="${FPC:-/usr/local/bin/fpc}"
SIGN_ID="${SIGN_ID:--}"   # "-" = ad-hoc; or a Developer ID identity name

if [ -z "${CI-}" ]; then
  ./install_deps.sh
fi

mkdir -p ../../Release/
sed -i.bak "s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver'/" ../../about.lfm

# Build (lazbuild also builds the required local package trcomp and produces the
# executable; the lpi carries the cocoa/-O- settings).
lazbuild -B ../../trcomp.lpk ../../transgui.lpi \
  --lazarusdir="$LAZARUS_DIR" --compiler="$FPC" --cpu="$CPU" --widgetset=cocoa
rc=$?

# Restore about.lfm regardless of build outcome.
mv ../../about.lfm.bak ../../about.lfm 2>/dev/null

if [ "$rc" != 0 ]; then
  echo "lazbuild failed (rc=$rc)"
  exit 1
fi

# lazbuild may place the binary in the project root or the unit output dir.
exename=""
for cand in ../../transgui ../../units/transgui; do
  if [ -e "$cand" ]; then exename="$cand"; break; fi
done
if [ -z "$exename" ]; then
  echo "built transgui binary not found"
  exit 1
fi
strip "$exename"

rm -rf "$appfolder"

echo "Creating $appfolder..."
mkdir -p "$appfolder/Contents/MacOS/lang"
mkdir -p "$appfolder/Contents/Resources"

mv "$exename" "$appfolder/Contents/MacOS/transgui"
cp ../../lang/transgui.* "$appfolder/Contents/MacOS/lang"

cp ../../history.txt "$dmgfolder"
cp ../../README.md "$dmgfolder"

cp PkgInfo "$appfolder/Contents"
cp transgui.icns "$appfolder/Contents/Resources"
sed -e "s/@prog_ver@/$prog_ver/" Info.plist > "$appfolder/Contents/Info.plist"

# Code-sign the finished bundle (required on Apple Silicon / macOS 26).
codesign --force --deep --options runtime --sign "$SIGN_ID" "$appfolder"
codesign --verify --verbose=2 "$appfolder" || { echo "codesign verification failed"; exit 1; }

ln -s /Applications "$dmgfolder/Drag \"Transmission Remote GUI\" here!"

hdiutil create -ov -anyowners -volname "transgui-v$prog_ver" -format UDRW -srcfolder ./Release -fs HFS+ "tmp.dmg"

mount_device="$(hdiutil attach -readwrite -noautoopen "tmp.dmg" | awk 'NR==1{print$1}')"
mount_volume="$(mount | grep "$mount_device" | sed 's/^[^ ]* on //;s/ ([^)]*)$//')"
cp transgui.icns "$mount_volume/.VolumeIcon.icns"
SetFile -c icnC "$mount_volume/.VolumeIcon.icns"
SetFile -a C "$mount_volume"

hdiutil detach "$mount_device"
rm -f "$dmg_dist_file"
hdiutil convert tmp.dmg -format UDBZ -imagekey zlib-level=9 -o "$dmg_dist_file"

rm tmp.dmg
rm -rf "$dmgfolder"

if [ -z "${CI-}" ]; then
  open "$dmg_dist_file"
fi
