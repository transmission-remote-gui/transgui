#!/bin/sh

set -x
set -e

# Install a Lazarus/FPC toolchain for building Transmission Remote GUI on macOS.
#
# Defaults target Apple Silicon (aarch64) using the Lazarus team's official
# "macOS aarch64" release builds. For Intel, set ARCH=x86-64 (and adjust the
# file names below to the matching x86-64 release).
#
# NOTE for macOS 26 (Tahoe): the released LCL still has Cocoa layout-recursion
# issues on Tahoe. If you hit UI instability, build the Lazarus development
# (main) branch instead and point create_app_new.sh at it via LAZARUS_DIR.

lazarus_ver="4.8"
fpc_dmg="fpc-3.2.4rc1a.intelarm64-macosx.dmg"
fpc_pkg_glob="fpc-*-macosx.pkg"
lazarus_zip="lazarus-darwin-aarch64-${lazarus_ver}.zip"
sf_base="https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20aarch64/Lazarus%20${lazarus_ver}"
lazarus_dest="${LAZARUS_DEST:-$HOME/lazarus}"

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

# --- FPC compiler (system install, needs sudo) ---
if [ ! -x "$(command -v fpc 2>&1)" ]; then
  curl -fL "${sf_base}/${fpc_dmg}?r=&ts=$(date +%s)${mirror_string-}" -o "fpc.dmg"
  mount_point="$(hdiutil attach -nobrowse -readonly fpc.dmg | awk 'END{$1="";$2="";sub(/^  */,"");print}')"
  sudo installer -pkg "$mount_point"/$fpc_pkg_glob -target /
  hdiutil detach "$mount_point"
  rm -f fpc.dmg
fi

# --- Lazarus IDE / LCL (portable zip, no sudo) ---
if [ ! -x "$(command -v lazbuild 2>&1)" ] && [ ! -x "$lazarus_dest/lazbuild" ]; then
  curl -fL "${sf_base}/${lazarus_zip}?r=&ts=$(date +%s)${mirror_string-}" -o "lazarus.zip"
  # Unmark quarantine before unzipping (required by the Lazarus macOS README).
  xattr -c lazarus.zip 2>/dev/null || true
  mkdir -p "$lazarus_dest"
  unzip -oq lazarus.zip -d "$lazarus_dest"
  rm -f lazarus.zip
  echo "Lazarus extracted to: $lazarus_dest"
  echo "Build with:  LAZARUS_DIR=\"$lazarus_dest\" ./create_app_new.sh"
fi
