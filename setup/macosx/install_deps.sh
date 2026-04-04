#!/bin/sh

set -e

# Install Lazarus and FPC via Homebrew (supports Apple Silicon + Intel)
if ! command -v brew >/dev/null 2>&1; then
  echo "Homebrew is required: https://brew.sh"
  exit 1
fi

if ! command -v lazbuild >/dev/null 2>&1; then
  brew install lazarus
fi
