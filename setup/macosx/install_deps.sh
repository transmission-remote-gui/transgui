#!/bin/sh

set -x
set -e

lazarus_ver="2.0.8"
fpc="fpc-3.0.4-macos-x86_64-laz-2"
lazarus="LazarusIDE-2.0.8-macos-x86_64"

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

download_and_verify_pkg() {
  url="$1"
  pkg="$2"

  wget "$url" -O "$pkg"

  if ! pkgutil --check-signature "$pkg"; then
    echo "Package signature verification failed for $pkg" >&2
    rm -f "$pkg"
    exit 1
  fi
}

if [ ! -x "$(command -v fpc 2>&1)" ]; then
  download_and_verify_pkg "https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}/$fpc.pkg?r=&ts=$(date +%s)${mirror_string-}" "fpc.pkg"
  sudo ln -s /usr/local/lib/fpc/3.0.4/ppcx64 /usr/local/bin/ppcx64
  sudo installer -pkg "fpc.pkg" -target /
  rm "fpc.pkg"
fi

if [ ! -x "$(command -v lazbuild 2>&1)" ]; then
  download_and_verify_pkg "https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}/$lazarus.pkg?r=&ts=$(date +%s)${mirror_string-}" "lazarus.pkg"
  sudo installer -pkg "lazarus.pkg" -target /
  rm "lazarus.pkg"
fi
