#!/bin/sh
fpc="fpc-3.0.4.intel-macosx"
lazarus="lazarus-1.8.0-i686-macosx"

if [ -n "${sourceforge_mirror-}" ]; then
    mirror_string="&use_mirror=$sourceforge_mirror"
fi

if [ ! -x "$(command -v fpc 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Mac%20OS%20X%20i386/Lazarus%201.8.0/$fpc.dmg?r=&ts=$(date +%s)${mirror_string-}" -O $fpc.dmg
  hdiutil attach -quiet $fpc.dmg
  pkgpath="$(hdiutil attach $fpc.dmg | grep Apple_HFS | awk '{ print $3 }')"
  sudo installer -pkg "$pkgpath/$fpc.pkg" -target /
  hdiutil unmount "$pkgpath"
  rm "$fpc.dmg"
fi

if [ ! -x "$(command -v lazbuild 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Mac%20OS%20X%20i386/Lazarus%201.8.0/$lazarus.dmg?r=&ts=$(date +%s)${mirror_string-}" -O "$lazarus.dmg"
  hdiutil attach -quiet "$lazarus.dmg"
  pkgpath="$(hdiutil attach "$lazarus.dmg" | grep Apple_HFS | awk '{ print $3 }')"
  sudo installer -pkg "$pkgpath/lazarus.pkg" -target /
  hdiutil unmount "$pkgpath"
  rm "$lazarus.dmg"
fi
