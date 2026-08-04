#!/bin/sh

set -x
set -e

lazarus_ver="2.0.8"
fpc="fpc-3.0.4-macos-x86_64-laz-2"
lazarus="LazarusIDE-2.0.8-macos-x86_64"
sourceforge_base="https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%20${lazarus_ver}"
download_attempts=3
download_timeout="${download_timeout:-600}"
download_backoff=5

if [ -n "${sourceforge_mirror-}" ]; then
  mirror_string="&use_mirror=${sourceforge_mirror}"
fi

cleanup_downloads() {
  rm -f "fpc.pkg.part" "lazarus.pkg.part"
}

trap cleanup_downloads EXIT

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

ppcx64_target="/usr/local/lib/fpc/3.0.4/ppcx64"
ppcx64_link="/usr/local/bin/ppcx64"

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

if [ ! -x "$(command -v fpc 2>&1)" ]; then
  validate_ppcx64_link
  download_package "$fpc.pkg" "fpc.pkg"
  sudo installer -pkg "fpc.pkg" -target /
  if [ ! -e "$ppcx64_link" ] && [ ! -L "$ppcx64_link" ]; then
    sudo ln -s "$ppcx64_target" "$ppcx64_link"
  else
    validate_ppcx64_link
  fi
  rm "fpc.pkg"
fi

if [ ! -x "$(command -v lazbuild 2>&1)" ]; then
  download_package "$lazarus.pkg" "lazarus.pkg"
  sudo installer -pkg "lazarus.pkg" -target /
  rm "lazarus.pkg"
fi
