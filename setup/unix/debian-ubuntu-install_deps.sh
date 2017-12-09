#!/bin/bash

set -ex

apt update -yqq
apt install -yqq lazarus xz-utils realpath git make jq zip
