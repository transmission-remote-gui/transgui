#!/bin/sh

set -x
set -e

lazarus_ver="2.0.8"
fpc="fpc-3.0.4-macos-x86_64-laz"
lazarus="LazarusIDE-2.0.8-macos-x86_64"

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

if [ ! -x "$(command -v fpc 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}/$fpc.pkg?r=&ts=$(date +%s)${mirror_string-}" -O "fpc.pkg"
  sudo ln -s /usr/local/lib/fpc/3.0.4/ppcx64 /usr/local/bin/ppcx64
  sudo installer -pkg "fpc.pkg" -target /
  rm "fpc.pkg"
fi

if [ ! -x "$(command -v lazbuild 2>&1)" ]; then
  wget "https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}/$lazarus.pkg?r=&ts=$(date +%s)${mirror_string-}" -O "lazarus.pkg"
  sudo installer -pkg "lazarus.pkg" -target /
  rm "lazarus.pkg"
fi
