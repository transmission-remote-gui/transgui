#!/usr/bin/env bash

set -e

./setup/macosx/install_deps.sh
lazbuild -B transgui.lpi --lazarusdir=/Library/Lazarus/ --compiler=/usr/local/bin/fpc --cpu=x86_64 --widgetset=cocoa
