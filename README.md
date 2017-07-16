# Transmission Remote GUI
[![Build Status](https://travis-ci.org/leonsoft-kras/transmisson-remote-gui.svg?branch=master)](https://travis-ci.org/leonsoft-kras/transmisson-remote-gui)

![Screenshot](http://i.imgur.com/Dum7Oka.png)

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
  - [Linux](#linux)
    - [Easy way (recommended)](#easy-way-recommended)
    - [Harder way](#harder-way)
  - [Windows](#windows)
  - [macOS](#macos)
- [Command line parameters](#command-line-parameters)
- [Portable mode](#portable-mode)
- [Fixed hotkeys](#fixed-hotkeys)
- [Advanced parameters](#advanced-parameters)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Introduction

Transmission Remote GUI is feature rich cross platform front-end to remotely control Transmission daemon via its RPC protocol. It is faster and has more functionality than builtin Transmission web interface.

Transmission Remote GUI is developed using Lazarus RAD and Free Pascal compiler.

Features:
 * Native application for Windows, Linux and macOS
 * uTorrent-like interface
 * Select files to download
 * Choose files priority
 * View details about connected peers
 * Full information about each torrent
 * Per torrent options

## Installation

### Linux

#### Easy way (recommended)

There are precompiled program's binaries for i386 and x86_64 Linux architectures.

 - Download and extract release for your architecture.
 - Create a desktop or menu shortcut to the transgui executable.
   * (If needed, change the transgui file permissions to executable).
 - Run the program using the created shortcut.

#### Harder way

Build the program by yourself.

 - Make sure you have working Lazarus and Free Pascal compiler installed.
   * Free Pascal Compiler 2.6.2+ and Lazarus 1.0 is used to develop Transmission Remote GUI.
 - Download the sources archive and extract it to some folder or perform svn checkout.
 - Open terminal/command line prompt and cd to the sources folder;
 - Execute "make" command to build the application;
 - Execute "make zipdist" command to create a release .zip archive in the "Release" sub-folder.

### Windows

 - Directly download the installer
 - Run the installer and follow the steps to install it on your system.

### macOS

 - Download the release image.
 - Open to mount the image.
 - Directly run the application or drag the app icon to your disk / Application folder.

## Command line parameters

You can specify path to a .torrent file or a magnet link as a command line parameter. The program will add the specified torrent.

 - `-hidden` : Start the program hidden. Only the program's tray icon will be visible.
 - `--home=<home_dir>` : Specifies a home directory for the program. All program's settings are stored in the home directory. You can run multiple instances of the program by specifying different home directories.

## Portable mode

If the program finds the transgui.ini file in the same folder as the binary file, then it will store all configuration and data files in the program's folder, instead of the folder in a user profile.

## Fixed hotkeys

 - Alt + 1 : All Torrents
 - Alt + 2 : Downloading
 - Alt + 3 : Completed
 - Alt + 4 : Active
 - Alt + 5 : Inactive
 - Alt + 6 : Stopped
 - Alt + 7 : Error
 - Alt + S : Searchbox (filter torrents by keywords) - Esc cancel filter and clean the box.
 - Alt + G : Info Pane - General Tab
 - Alt + K : Info Pane - Trackers Tab
 - Alt + P : Info Pane - Peers Tab
 - Alt + F : Info Pane - Files Tab

## Advanced parameters

There are some parameters in the transgui.ini file, that can not be modified via the GUI.
More info on: https://github.com/leonsoft-kras/transmisson-remote-gui/issues/924

```
[Interface]
; Maximum number of elements in the folder history list
MaxFoldersHistory=10

[Interface]
;In Linux/MacOs Only if "Open Container Folder" give you error
FileOpenDoc=0

[Interface]
;Alternate File Manager (Windows Only)
FileManagerDefault={Full path to your File Manager .exe}
FileManagerDefaultParam={Alternate parameters, could be left blank}

[Interface]
;System Wide Shortcut key (Windows Only)
GlobalHotkey={Virtual Key Code} full list here http://docwiki.embarcadero.com/RADStudio/Seattle/en/Virtual_Key_Codes (Plus VK_A...VK_Z and VK_0..VK_9)
GlobalHotkeyMod={Modifier Key} [MOD_SHIFT , MOD_CONTROL , MOD_ALT , MOD_WIN alone or combined with + sign]

[Shortcuts]
;Modify all the shortcuts of the GUI here
```

## License

Copyright (c) 2008-2017 by Yury Sidorov.

Transmission Remote GUI is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

Transmission Remote GUI is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
