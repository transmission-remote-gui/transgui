#!/bin/bash

set -euxo pipefail

LCL_WIDGETSET="${LCL_WIDGETSET:-gtk2}"
LAZARUS_DIR="${LAZARUS_DIR:-/usr/lib/lazarus/default}"

case "$LCL_WIDGETSET" in
  gtk2)
    widget_packages=(libgtk2.0-dev)
    ;;
  gtk3)
    widget_packages=(libgtk-3-dev pkg-config)
    ;;
  *)
    echo "Unsupported LCL_WIDGETSET: $LCL_WIDGETSET" >&2
    echo "Supported values: gtk2, gtk3" >&2
    exit 2
    ;;
esac

apt-get update -yqq
apt-get install -yqq --no-install-recommends \
  lazarus fpc xz-utils coreutils git make jq zip binutils libssl-dev \
  "${widget_packages[@]}"

fpc_full_target="$(fpc -iTP)-$(fpc -iTO)"
interfaces_ppu="$LAZARUS_DIR/lcl/units/$fpc_full_target/$LCL_WIDGETSET/interfaces.ppu"

if [ -f "$interfaces_ppu" ]; then
  exit 0
fi

interface_sources="$LAZARUS_DIR/lcl/interfaces/$LCL_WIDGETSET"
if [ ! -d "$interface_sources" ]; then
  echo "Lazarus LCL interface sources not found: $interface_sources" >&2
  exit 3
fi

make -C "$interface_sources" -j"$(nproc)" all

if [ ! -f "$interfaces_ppu" ]; then
  echo "Failed to build Lazarus LCL widgetset units: $interfaces_ppu" >&2
  exit 3
fi
