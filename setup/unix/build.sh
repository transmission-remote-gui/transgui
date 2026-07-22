#!/bin/bash

set -ex

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")/../../" && pwd)"
VERSION="$(cat "$ROOT/VERSION.txt")"
ARCH="$(uname -m)"

lazarus_dir_for_lazbuild() {
  local lazbuild_path="$1"
  local real_lazbuild
  local lazarus_dir

  real_lazbuild="$(readlink -f "$lazbuild_path" 2> /dev/null || realpath "$lazbuild_path")" || return 1
  [ -n "$real_lazbuild" ] || return 1

  lazarus_dir="$(dirname "$real_lazbuild")"
  if [ "$(basename "$lazarus_dir")" = "bin" ] && [ -d "$(dirname "$lazarus_dir")/lcl" ]; then
    lazarus_dir="$(dirname "$lazarus_dir")"
  fi
  printf '%s\n' "$lazarus_dir"
}

LCL_WIDGETSET="${LCL_WIDGETSET:-gtk2}"
if [ -z "${LAZARUS_DIR:-}" ]; then
  LAZARUS_DIR="/usr/lib/lazarus/default/"
  if command -v lazbuild > /dev/null 2>&1; then
    lazbuild_path="$(command -v lazbuild)"
    detected_lazarus_dir=""
    if [ -x "$lazbuild_path" ]; then
      detected_lazarus_dir="$(lazarus_dir_for_lazbuild "$lazbuild_path" || true)"
    fi
    if [ -n "$detected_lazarus_dir" ] && [ -d "$detected_lazarus_dir/lcl" ]; then
      LAZARUS_DIR="$detected_lazarus_dir"
    fi
  fi
  if [ ! -d "$LAZARUS_DIR/lcl" ]; then
    for lazarus_dir in /usr/lib/lazarus/default /usr/share/lazarus/default /usr/lib/lazarus/* /usr/share/lazarus/* /usr/lib/lazarus /usr/share/lazarus; do
      if [ -d "$lazarus_dir/lcl" ]; then
        LAZARUS_DIR="$lazarus_dir"
        break
      fi
    done
  fi
fi

if [ ! -d "$LAZARUS_DIR/lcl" ]; then
  echo "Lazarus directory not found or missing lcl: $LAZARUS_DIR" >&2
  exit 8
fi

LAZBUILD_CMD="lazbuild"
if [ -x "$LAZARUS_DIR/lazbuild" ]; then
  LAZBUILD_CMD="$LAZARUS_DIR/lazbuild"
elif [ -x "$LAZARUS_DIR/bin/lazbuild" ]; then
  LAZBUILD_CMD="$LAZARUS_DIR/bin/lazbuild"
else
  lazbuild_path="$(command -v lazbuild || true)"
  if [ -z "$lazbuild_path" ] || [ ! -x "$lazbuild_path" ]; then
    echo "lazbuild not found in $LAZARUS_DIR or PATH" >&2
    exit 9
  fi
  LAZBUILD_CMD="$lazbuild_path"
fi

lazbuild_dir="$(lazarus_dir_for_lazbuild "$LAZBUILD_CMD")" || {
  echo "Unable to resolve lazbuild path: $LAZBUILD_CMD" >&2
  exit 9
}
if [ "$(cd "$lazbuild_dir" && pwd -P)" != "$(cd "$LAZARUS_DIR" && pwd -P)" ]; then
  echo "lazbuild at $LAZBUILD_CMD does not match LAZARUS_DIR: $LAZARUS_DIR" >&2
  exit 9
fi

case "$LCL_WIDGETSET" in
  gtk2 | gtk3) ;;
  *)
    echo "Unsupported LCL_WIDGETSET: $LCL_WIDGETSET" >&2
    echo "Supported values: gtk2, gtk3" >&2
    exit 3
    ;;
esac

LAZARUS_PCP="$(mktemp -d "/tmp/lazarus-pcp-transgui-${ARCH}.XXXXXX")"

cleanup() {
  if [ -n "${about_tmp_file:-}" ] && [ -f "$about_tmp_file" ]; then
    rm -f "$about_tmp_file"
  fi
  if [ "${about_backup_created:-0}" = "1" ] && [ -f "$about_backup_file" ]; then
    mv "$about_backup_file" "$ROOT/about.lfm"
  fi
  if [ -n "${LAZARUS_PCP:-}" ] && [ -d "$LAZARUS_PCP" ]; then
    rm -rf "${LAZARUS_PCP:?}"
  fi
}
trap cleanup EXIT

cleanup_lazbuild_state() {
  rm -rf "${ROOT:?}/units" "${ROOT:?}/lib"
  mkdir -p "${ROOT:?}/units" "${ROOT:?}/lib" "${LAZARUS_PCP:?}"
}

has_lcl_widgetset_units() {
  [ -f "$LAZARUS_DIR/lcl/units/$FPC_FULL_TARGET/$LCL_WIDGETSET/interfaces.ppu" ]
}

prepare_lcl_widgetset() {
  if has_lcl_widgetset_units; then
    return
  fi

  if [ ! -d "$LAZARUS_DIR/lcl/interfaces/$LCL_WIDGETSET" ]; then
    echo "Lazarus LCL widgetset interface sources not found: $LCL_WIDGETSET" >&2
    exit 10
  fi

  make -C "$LAZARUS_DIR/lcl/interfaces/$LCL_WIDGETSET" -j"$NPROC" all

  if ! has_lcl_widgetset_units; then
    echo "Failed to build Lazarus LCL widgetset units: $LCL_WIDGETSET" >&2
    exit 10
  fi
}

run_lazbuild() {
  cleanup_lazbuild_state
  "$LAZBUILD_CMD" -B "$ROOT/trcomp.lpk" \
    --ws="$LCL_WIDGETSET" \
    --lazarusdir="$LAZARUS_DIR" \
    --pcp="$LAZARUS_PCP"
  "$LAZBUILD_CMD" -B "$ROOT/transgui.lpi" \
    --ws="$LCL_WIDGETSET" \
    --lazarusdir="$LAZARUS_DIR" \
    --pcp="$LAZARUS_PCP"
}

build="$(git rev-list --abbrev-commit --max-count=1 HEAD)"
lazarus_ver="$("$LAZBUILD_CMD" -v)"
fpc_ver="Free Pascal Compiler version $(fpc -iV)"
FPC_FULL_TARGET="$(fpc -iTP)-$(fpc -iTO)"
about_file="$ROOT/about.lfm"
version_caption="Caption = 'Version %s'"
build_caption="Caption = 'Version %s Build $build'#13#10'Compiled by: $fpc_ver, Lazarus v$lazarus_ver, LCL $LCL_WIDGETSET'"
about_backup_created=0
about_backup_file=""
about_tmp_file=""

if ! grep -Fq "$version_caption" "$about_file"; then
  echo "Unable to find version caption in $about_file" >&2
  exit 4
fi

about_backup_file="$(mktemp "$about_file.bak.XXXXXX")"
cp -p "$about_file" "$about_backup_file"
about_backup_created=1
about_tmp_file="$(mktemp "$about_file.XXXXXX")"
awk -v old="$version_caption" -v new="$build_caption" '
  index($0, "object txVersion: TLabel") {
    in_tx_version = 1
  }
  in_tx_version && !done {
    pos = index($0, old)
    if (pos) {
      $0 = substr($0, 1, pos - 1) new substr($0, pos + length(old))
      done = 1
    }
  }
  in_tx_version && /^[[:space:]]*end[[:space:]]*$/ {
    in_tx_version = 0
  }
  { print }
  END {
    if (!done) {
      exit 1
    }
  }
' "$about_backup_file" > "$about_tmp_file"
mv "$about_tmp_file" "$about_file"
about_tmp_file=""

if ! awk -v needle="LCL $LCL_WIDGETSET" '
  index($0, "object txVersion: TLabel") {
    in_tx_version = 1
  }
  in_tx_version && index($0, needle) {
    found = 1
  }
  in_tx_version && /^[[:space:]]*end[[:space:]]*$/ {
    in_tx_version = 0
  }
  END {
    exit found ? 0 : 1
  }
' "$about_file"; then
  echo "Failed to inject build metadata into $about_file" >&2
  exit 5
fi

NPROC="$(nproc 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null || getconf _NPROCESSORS_ONLN 2> /dev/null || echo 1)"

prepare_lcl_widgetset
run_lazbuild

make -C "$ROOT" -j"$NPROC" clean \
  LAZARUS_DIR="$LAZARUS_DIR" \
  LCL_WIDGETSET="$LCL_WIDGETSET"

make -C "$ROOT" -j"$NPROC" all \
  LAZARUS_DIR="$LAZARUS_DIR" \
  LCL_WIDGETSET="$LCL_WIDGETSET"

bash "$ROOT/setup/unix/verify_linux_binary.sh" "$ROOT/transgui" "$LCL_WIDGETSET"

cd "$ROOT" || exit 1
mkdir -p Release/
FILENAME="transgui-${VERSION}-$(uname -m)-$(uname)-${LCL_WIDGETSET}.txz"
XZ_OPT=-9 tar cJf "Release/$FILENAME" transgui README.md history.txt LICENSE transgui.png lang
