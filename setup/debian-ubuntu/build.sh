#!/bin/bash

set -ex

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}" )")/../../" && pwd)"

make -C "$ROOT" -j"$(nproc)"
make -C "$ROOT" -j"$(nproc)" zipdist
