#!/bin/bash

set -ex

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}" )")/../../" && pwd)"

build="$(git rev-list --abbrev-commit --max-count=1 HEAD)"
lazarus_ver="$(lazbuild -v)"
fpc_ver="$(fpc -i V | head -n 1)"


sed -i.bak "s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver'/" "$ROOT/about.lfm"

make -C "$ROOT" -j"$(nproc)"
make -C "$ROOT" -j"$(nproc)" zipdist

mv "$ROOT/about.lfm.bak" "$ROOT/about.lfm"
