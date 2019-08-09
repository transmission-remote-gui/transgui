#!/bin/bash

set -ex

apt update -yqq
apt install -yqq --no-install-recommends lazarus fpc xz-utils coreutils git make jq zip libssl-dev
