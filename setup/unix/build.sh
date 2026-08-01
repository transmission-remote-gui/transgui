#!/bin/bash

set -euxo pipefail

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")/../../" && pwd)"
VERSION="$(cat "$ROOT/VERSION.txt")"
ARCH="$(uname -m)"
LCL_WIDGETSET="${LCL_WIDGETSET:-gtk2}"
LAZARUS_DIR="${LAZARUS_DIR:-/usr/lib/lazarus/default}"
NPROC="$(nproc 2> /dev/null || getconf _NPROCESSORS_ONLN 2> /dev/null || echo 1)"

case "$LCL_WIDGETSET" in
  gtk2 | gtk3) ;;
  *)
    echo "Unsupported LCL_WIDGETSET: $LCL_WIDGETSET" >&2
    echo "Supported values: gtk2, gtk3" >&2
    exit 2
    ;;
esac

if [ ! -d "$LAZARUS_DIR/lcl" ]; then
  echo "Lazarus directory not found or missing lcl: $LAZARUS_DIR" >&2
  exit 3
fi

LAZBUILD_CMD="lazbuild"
if [ -x "$LAZARUS_DIR/lazbuild" ]; then
  LAZBUILD_CMD="$LAZARUS_DIR/lazbuild"
elif ! command -v "$LAZBUILD_CMD" > /dev/null 2>&1; then
  echo "lazbuild not found in $LAZARUS_DIR or PATH" >&2
  exit 3
fi

fpc_full_target="$(fpc -iTP)-$(fpc -iTO)"
interfaces_ppu="$LAZARUS_DIR/lcl/units/$fpc_full_target/$LCL_WIDGETSET/interfaces.ppu"
if [ ! -f "$interfaces_ppu" ]; then
  echo "Lazarus LCL widgetset units not found: $interfaces_ppu" >&2
  echo "Install or build the $LCL_WIDGETSET interface units before running this script." >&2
  exit 4
fi

LAZARUS_PCP="$(mktemp -d "/tmp/lazarus-pcp-transgui-${ARCH}.XXXXXX")"
about_backup=""
about_backup_ready=0

restore_about() {
  if [ -n "$about_backup" ] && [ -f "$about_backup" ]; then
    if [ "$about_backup_ready" = "1" ]; then
      if ! cp -p "$about_backup" "$ROOT/about.lfm"; then
        echo "Failed to restore $ROOT/about.lfm; backup retained at $about_backup" >&2
        return 1
      fi
    fi
    rm -f "$about_backup"
    about_backup=""
    about_backup_ready=0
  fi
}

cleanup() {
  status=$?
  set +e
  restore_about
  rm -rf "${LAZARUS_PCP:?}"
  trap - EXIT
  exit "$status"
}
trap cleanup EXIT

cleanup_lazbuild_state() {
  rm -rf "${ROOT:?}/units" "${ROOT:?}/lib"
  mkdir -p "${ROOT:?}/units" "${ROOT:?}/lib" "${LAZARUS_PCP:?}"
}

run_lazbuild() {
  cleanup_lazbuild_state
  "$LAZBUILD_CMD" -B "$ROOT/trcomp.lpk" --ws="$LCL_WIDGETSET" --lazarusdir="$LAZARUS_DIR" --pcp="$LAZARUS_PCP"
  "$LAZBUILD_CMD" -B "$ROOT/transgui.lpi" --ws="$LCL_WIDGETSET" --lazarusdir="$LAZARUS_DIR" --pcp="$LAZARUS_PCP"
}

build="$(git -C "$ROOT" rev-list --abbrev-commit --max-count=1 HEAD)"
lazarus_ver="$("$LAZBUILD_CMD" -v)"
fpc_ver="Free Pascal Compiler version $(fpc -iV)"

about_backup="$(mktemp "/tmp/transgui-about-${ARCH}.XXXXXX")"
if ! cp -p "$ROOT/about.lfm" "$about_backup"; then
  echo "Failed to back up $ROOT/about.lfm" >&2
  exit 5
fi
about_backup_ready=1
sed -i "/^[[:space:]]*object txVersion: TLabel$/,/^[[:space:]]*end[[:space:]]*$/ s/'Version %s'/'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver, LCL $LCL_WIDGETSET'/" "$ROOT/about.lfm"

if ! sed -n "/^[[:space:]]*object txVersion: TLabel$/,/^[[:space:]]*end[[:space:]]*$/p" "$ROOT/about.lfm" | grep -Fq "LCL $LCL_WIDGETSET"; then
  echo "Failed to inject build metadata into $ROOT/about.lfm" >&2
  exit 5
fi

run_lazbuild
make -C "$ROOT" -j"$NPROC" clean LAZARUS_DIR="$LAZARUS_DIR" LCL_WIDGETSET="$LCL_WIDGETSET"
make -C "$ROOT" -j"$NPROC" all LAZARUS_DIR="$LAZARUS_DIR" LCL_WIDGETSET="$LCL_WIDGETSET"

if [ "$(uname -s)" = "Linux" ]; then
  "$ROOT/setup/unix/verify_linux_binary.sh" "$ROOT/transgui" "$LCL_WIDGETSET"
fi

restore_about

cd "$ROOT" || exit 1
mkdir -p Release/
widgetset_suffix=""
if [ "$LCL_WIDGETSET" = "gtk3" ]; then
  widgetset_suffix="-gtk3"
fi
FILENAME="transgui-${VERSION}-$(uname -m)-$(uname)${widgetset_suffix}.txz"
XZ_OPT=-9 tar cJf "Release/$FILENAME" transgui README.md history.txt LICENSE transgui.png lang
