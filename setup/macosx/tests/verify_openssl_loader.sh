#!/bin/sh

set -eu

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <OpenSSL prefix>" >&2
  exit 2
fi

root_dir="$(cd -P -- "$(dirname "$0")/../../.." && pwd)"
build_dir="$(mktemp -d "${TMPDIR:-/tmp}/transgui-openssl-loader.XXXXXX")"

cleanup() {
  rm -rf "$build_dir"
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM

mkdir -p "$build_dir/units"
mkdir -p "$build_dir/override/lib"
ln -s "$1/lib/libcrypto.3.dylib" \
  "$build_dir/override/lib/libcrypto.3.dylib"
ln -s "$1/lib/libssl.3.dylib" \
  "$build_dir/override/lib/libssl.3.dylib"
fpc -B \
  -Fu"$root_dir/synapse/source/lib" \
  -FE"$build_dir" \
  -FU"$build_dir/units" \
  "$root_dir/setup/macosx/tests/openssl_loader_probe.pas"
"$build_dir/openssl_loader_probe" "$1"
"$build_dir/openssl_loader_probe" "$build_dir/override" override
"$build_dir/openssl_loader_probe" "$build_dir/override" invalid-override
