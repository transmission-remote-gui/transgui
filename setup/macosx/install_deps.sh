#!/bin/sh

set -e

# Install FPC via Homebrew cask (universal binary, works on Apple Silicon + Intel)
if ! command -v fpc >/dev/null 2>&1; then
  brew install --cask fpc-laz
fi

# Install Lazarus IDE directly from SourceForge (Homebrew lazarus cask is deprecated)
if ! command -v lazbuild >/dev/null 2>&1 && [ ! -x /Applications/Lazarus/lazbuild ]; then
  curl -fsSL "https://downloads.sourceforge.net/project/lazarus/Lazarus%20macOS%20x86-64/Lazarus%203.6/Lazarus-3.6-macosx-x86_64.pkg" -o /tmp/Lazarus-3.6.pkg
  sudo installer -pkg /tmp/Lazarus-3.6.pkg -target /
  rm /tmp/Lazarus-3.6.pkg
fi
