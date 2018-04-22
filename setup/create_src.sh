#!/bin/sh

prog_ver="$(cat ../VERSION)"
zipfile="../Release/transgui-$prog_ver-src.zip"

mkdir -p "../Release"
rm -rf TransGUI

svn export .. TransGUI
rm -f "$zipfile"
zip -9 -r "$zipfile" TransGUI

rm -r TransGUI
