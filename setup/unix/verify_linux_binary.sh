#!/bin/bash

set -euo pipefail
export LC_ALL=C

binary="${1:-./transgui}"
expected_widgetset="${2:-${LCL_WIDGETSET:-gtk2}}"

case "$expected_widgetset" in
  gtk2)
    expected_library="libgtk-x11-2.0.so.0"
    unexpected_library="libgtk-3.so.0"
    ;;
  gtk3)
    expected_library="libgtk-3.so.0"
    unexpected_library="libgtk-x11-2.0.so.0"
    ;;
  *)
    echo "Unsupported expected widgetset: $expected_widgetset" >&2
    exit 2
    ;;
esac

if [ ! -x "$binary" ]; then
  echo "Binary not found or not executable: $binary" >&2
  exit 2
fi

if ! command -v readelf > /dev/null 2>&1; then
  echo "readelf not found; install binutils to inspect shared libraries." >&2
  exit 3
fi

needed_libraries="$(
  readelf -dW "$binary" |
    sed -n 's/^.*Shared library: \[\([^]]*\)\].*$/\1/p'
)"

if ! grep -Fxq "$expected_library" <<< "$needed_libraries"; then
  echo "Expected $expected_widgetset linkage, but $expected_library was not found." >&2
  exit 4
fi

if grep -Fxq "$unexpected_library" <<< "$needed_libraries"; then
  echo "Expected $expected_widgetset linkage, but $unexpected_library was also found." >&2
  exit 4
fi

echo "Binary uses the expected $expected_widgetset linkage."
