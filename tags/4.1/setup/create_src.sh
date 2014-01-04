#!/bin/sh

prog_ver=`cat ../VERSION.txt`
zipfile=../Release/transgui-$prog_ver-src.zip

if [ ! -e "../Release" ]
then
  mkdir "../Release"
fi

if [ -e TransGUI ]
then
  rm -r TransGUI
fi

svn export .. TransGUI
rm $zipfile
zip -9 -r $zipfile TransGUI

rm -r TransGUI
