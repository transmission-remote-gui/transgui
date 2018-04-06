#!/bin/bash

set -ex

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}" )")/../../" && pwd)"
VERSION="$(cat "$ROOT/VERSION.txt")"

build="$(git rev-list --abbrev-commit --max-count=1 HEAD)"
lazarus_ver="$(lazbuild -v)"
fpc_ver="$(fpc -i V | head -n 1)"


sed -i.bak "s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver'/" "$ROOT/about.lfm"

lazbuild -B "$ROOT/transgui.lpi" --lazarusdir=/usr/lib/lazarus/default/
make -C "$ROOT" -j"$(nproc)" clean
make -C "$ROOT" -j"$(nproc)" all

mv "$ROOT/about.lfm.bak" "$ROOT/about.lfm"

cd "$ROOT" || exit 1
mkdir -p Release/
FILENAME="transgui-${VERSION}-$(uname -m)-$(uname).txz"
XZ_OPT=-9 tar cJf "Release/$FILENAME" transgui README.md history.txt LICENSE transgui.png lang
