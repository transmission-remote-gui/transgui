#!/bin/sh

set -x
set -e

lazarus_ver="2.0.4"
fpc="fpc-3.0.4a.intel-macosx"
lazarus="lazarus-2.0.4-i686-macosx"

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

if [ ! -x "$(command -v fpc 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Mac%20OS%20X%20i386/Lazarus%20${lazarus_ver}/$fpc.dmg?r=&ts=$(date +%s)${mirror_string-}" -O "$fpc.dmg"
  hdiutil attach -quiet "$fpc.dmg"
  pkgpath="$(hdiutil attach "$fpc.dmg" | command awk "/Apple_HFS/ { print \$3 }")"
  sudo installer -pkg "$pkgpath/$fpc.pkg" -target /
  hdiutil unmount "$pkgpath"
  rm "$fpc.dmg"
fi

if [ ! -x "$(command -v lazbuild 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Mac%20OS%20X%20i386/Lazarus%20${lazarus_ver}/$lazarus.dmg?r=&ts=$(date +%s)${mirror_string-}" -O "$lazarus.dmg"
  hdiutil attach -quiet "$lazarus.dmg"
  pkgpath="$(hdiutil attach "$lazarus.dmg" | command awk "/Apple_HFS/ { print \$3 }")"
  sudo installer -pkg "$pkgpath/lazarus.pkg" -target /
  hdiutil unmount "$pkgpath"
  rm "$lazarus.dmg"
  lazbuild --build-ide= --compiler=fpc --cpu=x86_64 --widgetset=cocoa
fi
