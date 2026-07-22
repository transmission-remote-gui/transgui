#!/bin/bash

set -ex

LCL_WIDGETSET="${LCL_WIDGETSET:-gtk2}"

base_pkgs=(
  lazarus
  fpc
  xz-utils
  coreutils
  git
  make
  jq
  zip
  binutils
  pkg-config
  libssl-dev
)

case "$LCL_WIDGETSET" in
  gtk2)
    widget_pkgs=(libgtk2.0-dev)
    ;;
  gtk3)
    widget_pkgs=(libgtk-3-dev)
    ;;
  *)
    echo "Unsupported LCL_WIDGETSET: $LCL_WIDGETSET" >&2
    echo "Supported values: gtk2, gtk3" >&2
    exit 2
    ;;
esac

apt update -yqq
apt install -yqq --no-install-recommends \
  "${base_pkgs[@]}" \
  "${widget_pkgs[@]}"

for lazarus_dir in /usr/lib/lazarus /usr/share/lazarus; do
  if [ -d "$lazarus_dir" ]; then
    if find "$lazarus_dir" -type d -path "*/lcl/units/*/${LCL_WIDGETSET}" -print -quit | grep -q .; then
      exit 0
    fi

    interface_dir="$(find "$lazarus_dir" -type d -path "*/lcl/interfaces/${LCL_WIDGETSET}" -print -quit || true)"
    if [ -n "$interface_dir" ] &&
      find "$interface_dir" -type f \( -name '*.pas' -o -name '*.pp' \) -print -quit | grep -q .; then
      echo "Prebuilt Lazarus LCL widgetset units not found: ${LCL_WIDGETSET}" >&2
      echo "Lazarus sources are available; build.sh will compile them during build." >&2
      exit 0
    fi
  fi
done

echo "Lazarus LCL widgetset units not found: ${LCL_WIDGETSET}" >&2
echo "Please ensure the Lazarus package for ${LCL_WIDGETSET} is installed." >&2
exit 3
