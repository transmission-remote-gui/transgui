#!/bin/bash

set -ex

case "${1-}" in
  debian)
    printf '%s\n' \
      'Acquire::Check-Valid-Until "false";' \
      'Acquire::Retries "5";' > /etc/apt/apt.conf.d/99archive
    for source_file in /etc/apt/sources.list /etc/apt/sources.list.d/*.list; do
      [ -f "$source_file" ] || continue
      sed -i \
        -e "s|http://deb.debian.org/debian|http://archive.debian.org/debian|g" \
        -e "s|http://security.debian.org/debian-security|http://archive.debian.org/debian-security|g" \
        -e "s|http://deb.debian.org/debian-security|http://archive.debian.org/debian-security|g" \
        "$source_file"
    done
    ;;
  raspbian)
    printf '%s\n' \
      'Acquire::Check-Valid-Until "false";' \
      'Acquire::Retries "5";' > /etc/apt/apt.conf.d/99archive
    for source_file in /etc/apt/sources.list /etc/apt/sources.list.d/*.list; do
      [ -f "$source_file" ] || continue
      sed -i \
        -e "s|http://archive.raspbian.org/raspbian|http://legacy.raspbian.org/raspbian|g" \
        -e "s|http://raspbian.raspberrypi.org/raspbian|http://legacy.raspbian.org/raspbian|g" \
        -e "s|http://mirrordirector.raspbian.org/raspbian|http://legacy.raspbian.org/raspbian|g" \
        -e "s|http://mirrors.ocf.berkeley.edu/raspbian/raspbian|http://legacy.raspbian.org/raspbian|g" \
        "$source_file"
    done
    ;;
  *)
    echo "Usage: $0 debian|raspbian" >&2
    exit 1
    ;;
esac
