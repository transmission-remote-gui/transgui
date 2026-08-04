#!/bin/sh

set -ex

prog_ver="$(cat ../../VERSION.txt)"
build="$(git rev-list --abbrev-commit --max-count=1 HEAD ../..)"
fpc_ver="$(fpc -i V | head -n 1)"
exename=../../transgui
appname="Transmission Remote GUI"
dmg_dist_file="../../Release/transgui-$prog_ver.dmg"
dmg_temp_file="$dmg_dist_file.tmp.dmg"
dmgfolder=./Release
appfolder="$dmgfolder/$appname.app"
lazarus_dir="${1:-/Library/Lazarus/}"
lazbuild_bin="$lazarus_dir/lazbuild"
if [ ! -x "$lazbuild_bin" ]; then
  echo "Unable to find lazbuild at $lazbuild_bin." >&2
  exit 1
fi
lazarus_ver="$("$lazbuild_bin" -v)"
mount_device=""
remove_dmg_temp=0
remove_dmgfolder=0
remove_tmp_dmg=0
lazarus_pcp=""

detach_disk_image() {
  detach_device=$1
  detach_attempt=1

  sync
  while ! hdiutil detach "$detach_device"; do
    if [ "$detach_attempt" -ge 5 ]; then
      echo "Failed to detach $detach_device after $detach_attempt attempts." >&2
      return 1
    fi

    echo "Failed to detach $detach_device; retrying in 2 seconds." >&2
    sleep 2
    detach_attempt=$((detach_attempt + 1))
  done
}

cleanup() {
  status=$?
  cleanup_status=0

  trap '' HUP INT TERM
  trap - 0
  set +e

  if [ -n "$mount_device" ]; then
    detach_status=0
    detach_disk_image "$mount_device" || detach_status=$?
    if [ "$detach_status" -eq 0 ]; then
      mount_device=""
    else
      cleanup_status=$detach_status
      remove_tmp_dmg=0
    fi
  fi
  if [ "$remove_dmg_temp" -eq 1 ]; then
    rm -f "$dmg_temp_file" || cleanup_status=$?
  fi
  if [ "$remove_tmp_dmg" -eq 1 ]; then
    rm -f tmp.dmg || cleanup_status=$?
  fi
  if [ "$remove_dmgfolder" -eq 1 ]; then
    rm -rf "$dmgfolder" || cleanup_status=$?
  fi
  if [ -e ../../about.lfm.bak ]; then
    mv ../../about.lfm.bak ../../about.lfm || cleanup_status=$?
  fi
  if [ -n "$lazarus_pcp" ]; then
    rm -rf "$lazarus_pcp" || :
  fi

  if [ "$status" -eq 0 ]; then
    status=$cleanup_status
  fi
  exit "$status"
}

if [ -z "${CI-}" ]; then
  ./install_deps.sh
fi

if [ ! "$lazarus_dir" = "" ]; then
  lazdir=LAZARUS_DIR="$lazarus_dir"
fi

if [ -e ../../about.lfm.bak ]; then
  echo "../../about.lfm.bak already exists; refusing to overwrite it." >&2
  exit 1
fi
if [ -e "$dmg_dist_file" ] && [ ! -f "$dmg_dist_file" ]; then
  echo "$dmg_dist_file exists and is not a regular file." >&2
  exit 1
fi
if [ -e "$dmg_temp_file" ] && [ ! -f "$dmg_temp_file" ]; then
  echo "$dmg_temp_file exists and is not a regular file." >&2
  exit 1
fi

trap cleanup 0
trap 'exit 1' HUP INT TERM

lazarus_pcp="$(mktemp -d "${TMPDIR:-/tmp}/transgui-lazarus-pcp.XXXXXX")" || exit 1

cleanup_lazbuild_state() {
  rm -rf ../../units ../../lib
  mkdir -p ../../units ../../lib "$lazarus_pcp"
}

run_lazbuild() {
  cleanup_lazbuild_state
  "$lazbuild_bin" -B ../../trcomp.lpk --lazarusdir="$lazarus_dir" --compiler=/usr/local/bin/fpc --cpu=x86_64 --widgetset=cocoa --pcp="$lazarus_pcp"
  "$lazbuild_bin" -B ../../transgui.lpi --lazarusdir="$lazarus_dir" --compiler=/usr/local/bin/fpc --cpu=x86_64 --widgetset=cocoa --pcp="$lazarus_pcp"
}

mkdir -p ../../Release/
sed -i.bak "s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver'/" ../../about.lfm

run_lazbuild

# Building Intel version
make -j"$(sysctl -n hw.ncpu)" -C ../.. clean CPU_TARGET=x86_64 "$lazdir"
make -j"$(sysctl -n hw.ncpu)" -C ../.. CPU_TARGET=x86_64 "$lazdir"

if ! [ -e $exename ]; then
  echo "$exename does not exist"
  exit 1
fi
strip "$exename"

remove_dmgfolder=1
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
remove_tmp_dmg=1

remove_tmp_dmg=0
attach_status=0
attach_output="$(hdiutil attach -readwrite -noautoopen "tmp.dmg")" || attach_status=$?
mount_device="$(printf '%s\n' "$attach_output" | awk 'NR == 1 && $1 ~ /^\/dev\/disk[0-9]+(s[0-9]+)*$/ {print $1}')"
if [ -z "$mount_device" ]; then
  echo "Failed to determine the mounted disk image device." >&2
  if [ "$attach_status" -ne 0 ]; then
    exit "$attach_status"
  fi
  exit 1
fi
remove_tmp_dmg=1
if [ "$attach_status" -ne 0 ]; then
  exit "$attach_status"
fi
mount_volume="$(printf '%s\n' "$attach_output" | awk -F '\t' 'NF >= 3 && $NF ~ /^\// {volume=$NF} END {print volume}')"
if [ -z "$mount_volume" ]; then
  echo "Failed to determine the mounted disk image volume." >&2
  exit 1
fi
cp transgui.icns "$mount_volume/.VolumeIcon.icns"
SetFile -c icnC "$mount_volume/.VolumeIcon.icns"
SetFile -a C "$mount_volume"

detach_disk_image "$mount_device"
mount_device=""
remove_dmg_temp=1
rm -f "$dmg_temp_file"
hdiutil convert tmp.dmg -format UDBZ -imagekey zlib-level=9 -o "$dmg_temp_file"
mv -f "$dmg_temp_file" "$dmg_dist_file"
remove_dmg_temp=0

if [ -z "${CI-}" ]; then
  open "$dmg_dist_file"
fi
