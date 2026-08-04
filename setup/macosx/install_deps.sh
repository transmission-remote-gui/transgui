#!/bin/sh

set -x
set -e

lazarus_ver="2.2.6"
fpc_ver="3.2.2"
fpc="fpc-${fpc_ver}.intelarm64-macosx"
lazarus="Lazarus-${lazarus_ver}-0-x86_64-macosx"
sourceforge_base="https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}"
download_attempts=3
download_timeout="${download_timeout:-600}"
download_backoff=5
fpc_mount=""
fpc_mounted=0
fpc_downloaded=0

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

detach_disk_image() {
  detach_device=$1
  detach_attempt=1

  sync
  while ! hdiutil detach "$detach_device"; do
    if [ "$detach_attempt" -ge 5 ]; then
      echo "Failed to detach $detach_device after $detach_attempt attempts." >&2
      return 1
    fi

    echo "Failed to detach $detach_device; retrying in 2 seconds." >&2
    sleep 2
    detach_attempt=$((detach_attempt + 1))
  done
}

cleanup_downloads() {
  if [ "$fpc_mounted" -eq 1 ]; then
    if detach_disk_image "$fpc_mount" > /dev/null; then
      fpc_mounted=0
    fi
  fi
  if [ "$fpc_mounted" -eq 0 ] && [ -n "$fpc_mount" ]; then
    rmdir "$fpc_mount" > /dev/null 2>&1 || true
  fi
  if [ "$fpc_mounted" -eq 0 ] && [ "$fpc_downloaded" -eq 1 ]; then
    rm -f "fpc.dmg" || true
  fi
  rm -f "fpc.dmg.part" "lazarus.pkg.part" || true
}

trap cleanup_downloads EXIT
trap 'exit 1' HUP INT TERM

download_url() {
  download_url_value="$1"
  download_destination="$2"
  partial="${download_destination}.part"
  attempt=1

  while [ "$attempt" -le "$download_attempts" ]; do
    rm -f "$partial"
    if curl --fail --location \
      --connect-timeout 30 --max-time "$download_timeout" \
      --output "$partial" "$download_url_value"; then
      mv "$partial" "$download_destination" || return 1
      return 0
    fi

    if [ "$attempt" -lt "$download_attempts" ]; then
      next_attempt=$((attempt + 1))
      echo "Download failed; retrying ($next_attempt/$download_attempts)" >&2
      sleep $((attempt * download_backoff))
    fi
    attempt=$((attempt + 1))
  done

  echo "Download failed after $download_attempts attempts: $download_url_value" >&2
  rm -f "$partial"
  return 1
}

download_package() {
  package="$1"
  destination="$2"
  package_url="${sourceforge_base}/${package}?r=&ts=$(date +%s)"

  rm -f "${destination}.part"

  if [ -n "${mirror_string-}" ]; then
    if download_url "${package_url}${mirror_string}" "$destination"; then
      return 0
    fi

    echo "Preferred SourceForge mirror failed; retrying automatic selection" >&2
    rm -f "${destination}.part"
  fi

  download_url "$package_url" "$destination"
}

ppcx64_target="/usr/local/lib/fpc/${fpc_ver}/ppcx64"
ppcx64_link="/usr/local/bin/ppcx64"
fpc_bin="/usr/local/bin/fpc"
lazbuild_target="/Applications/Lazarus/lazbuild"
lazbuild_link="/usr/local/bin/lazbuild"

validate_ppcx64_link() {
  if [ -L "$ppcx64_link" ]; then
    ppcx64_link_target="$(readlink "$ppcx64_link")"
    case "$ppcx64_link_target" in
      /*) ppcx64_target_dir="$(dirname "$ppcx64_link_target")" ;;
      *) ppcx64_target_dir="$(dirname "$ppcx64_link")/$(dirname "$ppcx64_link_target")" ;;
    esac
    if ppcx64_target_dir="$(cd -P "$ppcx64_target_dir" 2> /dev/null && pwd)" &&
      [ "$ppcx64_target_dir/$(basename "$ppcx64_link_target")" = "$ppcx64_target" ]; then
      return 0
    fi
    echo "$ppcx64_link exists and does not resolve to $ppcx64_target" >&2
    return 1
  fi

  if [ -e "$ppcx64_link" ]; then
    echo "$ppcx64_link exists and is not a symlink" >&2
    return 1
  fi

  return 0
}

if [ ! -x "$fpc_bin" ] || [ "$("$fpc_bin" -iV)" != "$fpc_ver" ] || [ ! -x "$ppcx64_target" ]; then
  if [ -e "$ppcx64_link" ] && [ ! -L "$ppcx64_link" ]; then
    validate_ppcx64_link
  fi
  download_package "$fpc.dmg" "fpc.dmg"
  fpc_downloaded=1
  fpc_mount="$(mktemp -d "${TMPDIR:-/tmp}/transgui-fpc.XXXXXX")"
  hdiutil attach -nobrowse -readonly -mountpoint "$fpc_mount" "fpc.dmg"
  fpc_mounted=1
  fpc_pkg="$fpc_mount/fpc-${fpc_ver}-intelarm64-macosx.mpkg"
  if [ ! -e "$fpc_pkg" ]; then
    echo "No installer package found in fpc.dmg" >&2
    exit 1
  fi
  sudo installer -pkg "$fpc_pkg" -target /
  detach_disk_image "$fpc_mount"
  fpc_mounted=0
  cleanup_downloads
fi

if [ ! -x "$ppcx64_target" ]; then
  echo "$ppcx64_target is missing or is not executable" >&2
  exit 1
fi
if ! validate_ppcx64_link; then
  if [ ! -L "$ppcx64_link" ]; then
    exit 1
  fi
  sudo rm "$ppcx64_link"
fi
if [ ! -L "$ppcx64_link" ]; then
  sudo ln -s "$ppcx64_target" "$ppcx64_link"
fi
validate_ppcx64_link

if [ ! -x "$lazbuild_target" ] || [ "$("$lazbuild_target" -v)" != "$lazarus_ver" ]; then
  download_package "$lazarus.pkg" "lazarus.pkg"
  sudo installer -pkg "lazarus.pkg" -target /
  rm "lazarus.pkg"
fi

if [ ! -x "$lazbuild_target" ]; then
  echo "$lazbuild_target is missing or is not executable" >&2
  exit 1
fi
if [ -e "$lazbuild_link" ] && [ ! -L "$lazbuild_link" ]; then
  echo "$lazbuild_link exists and is not a symlink" >&2
  exit 1
fi
if [ -L "$lazbuild_link" ] && [ "$(readlink "$lazbuild_link")" != "$lazbuild_target" ]; then
  sudo rm "$lazbuild_link"
fi
if [ ! -L "$lazbuild_link" ]; then
  sudo ln -s "$lazbuild_target" "$lazbuild_link"
fi

test "$("$fpc_bin" -iV)" = "$fpc_ver"
test "$("$lazbuild_link" -v)" = "$lazarus_ver"
