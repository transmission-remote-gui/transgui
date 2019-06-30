#!/bin/bash

set -ex

apt update -yqq
apt install -yqq lazarus xz-utils coreutils git make jq zip libssl-dev
