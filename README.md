# Transmission Remote GUI

[![CI](https://github.com/transmission-remote-gui/transgui/actions/workflows/ci.yml/badge.svg)](https://github.com/transmission-remote-gui/transgui/actions/workflows/ci.yml)

![Screenshot](https://i.imgur.com/XBbF4Vh.png)

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
  - [Linux](#linux)
    - [Easy way (recommended)](#easy-way-recommended)
    - [Harder way](#harder-way)
  - [Windows](#windows)
    - [Portable zip archive (recommended)](#portable-zip-archive-recommended)
    - [Installer](#installer)
    - [Using Chocolatey](#using-chocolatey)
  - [macOS](#macos)
    - [Without a package manager](#without-a-package-manager)
    - [Homebrew](#homebrew)
- [Command line parameters](#command-line-parameters)
- [Portable mode](#portable-mode)
- [Fixed hotkeys](#fixed-hotkeys)
- [Advanced parameters](#advanced-parameters)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Introduction

Transmission Remote GUI is a feature-rich, cross-platform front-end to remotely control a Transmission daemon via its RPC protocol. It is faster and has more functionality than the built-in Transmission web interface.

Transmission Remote GUI is developed using Lazarus RAD and the Free Pascal compiler.

Features:

- Native application for Windows, Linux and macOS
- uTorrent-like interface
- Select files to download
- Choose file priority
- View details about connected peers
- Full information about each torrent
- Per-torrent options

## Installation

The installers are listed on the GitHub [Releases](https://github.com/transmission-remote-gui/transgui/releases/latest) page, and the installation instructions for different platforms are listed below :arrow_down:

### Linux

#### Easy way (recommended)

Precompiled binaries for supported Linux architectures are available on the Releases page.

- Download and extract the release for your architecture.

Now you can execute the `transgui` binary. *(Make the `transgui` file executable if needed.)*

Additionally, you can create a desktop or menu shortcut to the transgui executable and run the program using the created shortcut.

#### Harder way

Build the program by yourself.

1. Make sure you have Lazarus and the Free Pascal compiler installed and working.
2. Download the source archive and extract it to a folder, or clone the repository.
3. Open a terminal/command-line prompt and cd to the source folder.
4. Execute the `lazbuild -B transgui.lpi --lazarusdir=<lazarus_dir>` command to build the transgui.res file, replacing `<lazarus_dir>` with your Lazarus installation path.
5. Execute the `make LAZARUS_DIR=<lazarus_dir>` command to build the application.
6. Execute the `make LAZARUS_DIR=<lazarus_dir> zipdist` command to create a release .zip archive in the `Release` sub-folder.

### Windows

#### Portable zip archive (recommended)

- The zip archive release is much smaller than the installer one, which can save you some bandwidth, disk space and time. Simply download and extract the zip archive wherever you want, then run `transgui.exe` directly or create a shortcut to it.

**Note**: If you need SSL/TLS support with a portable release, download the source archive for the [tag matching your release](https://github.com/transmission-remote-gui/transgui/tags), then copy the project's OpenSSL DLLs from `setup/win/openssl` for 32-bit or `setup/win_amd64/openssl` for 64-bit into the same folder as `transgui.exe`. If Windows reports that `MSVCR120.dll` is missing, install the Microsoft Visual C++ 2013 Redistributable; if `VCRUNTIME140.dll` is missing, install the latest v14 Redistributable. Download the package matching your build architecture from [Microsoft's supported Visual C++ Redistributable downloads](https://learn.microsoft.com/cpp/windows/latest-supported-vc-redist). Do not mix files from different releases or architectures.

#### Installer

This installer has an additional installation wizard and bundles the OpenSSL DLLs in every release; its size is much larger than the zip archive, but you don't need to take care of the OpenSSL dependencies.

1. Directly download the installer.
2. Run the installer and follow the steps to install it on your system.

#### Using [Chocolatey](https://chocolatey.org)

Run `choco install transgui` to install the latest version of Transmission Remote GUI.

### macOS

#### Without a package manager

This method requires no additional prerequisites or dependencies:

1. Download the disk image from the release page.
2. Open the image file to mount the image.
3. Directly run the application or drag the app icon to your disk / Applications folder.

#### Homebrew

You need to have [Homebrew](https://brew.sh/) installed. Execute this command to install Transmission Remote GUI:

- `brew install --cask transmission-remote-gui`

## Command line parameters

You can specify a path to a .torrent file or a magnet link as a command-line parameter. The program will add the specified torrent.

- `-hidden`: Start the program hidden. Only the program's tray icon will be visible.
- `--home=<home_dir>`: Specifies a home directory for the program. All the program's settings are stored in the home directory. You can run multiple instances of the program by specifying different home directories. This option takes precedence over portable mode and the default user-profile location.

## Portable mode

If the program finds the transgui.ini file in the same folder as the binary file, then it will store all configuration and data files in the program's folder, instead of the folder in a user profile.

## Fixed hotkeys

- <kbd>Alt</kbd> + <kbd>1</kbd>: All Torrents
- <kbd>Alt</kbd> + <kbd>2</kbd>: Downloading
- <kbd>Alt</kbd> + <kbd>3</kbd>: Completed
- <kbd>Alt</kbd> + <kbd>4</kbd>: Active
- <kbd>Alt</kbd> + <kbd>5</kbd>: Inactive
- <kbd>Alt</kbd> + <kbd>6</kbd>: Stopped
- <kbd>Alt</kbd> + <kbd>7</kbd>: Error
- <kbd>Alt</kbd> + <kbd>8</kbd>: Waiting
- <kbd>Alt</kbd> + <kbd>S</kbd>: Search box (filter torrents by keywords) - Esc cancels the filter and clears the box.
- <kbd>Alt</kbd> + <kbd>G</kbd>: Info Pane - General Tab
- <kbd>Alt</kbd> + <kbd>K</kbd>: Info Pane - Trackers Tab
- <kbd>Alt</kbd> + <kbd>P</kbd>: Info Pane - Peers Tab
- <kbd>Alt</kbd> + <kbd>F</kbd>: Info Pane - Files Tab

## Advanced parameters

There are some parameters in the `transgui.ini` file that cannot be modified via the GUI.
More info on: [#924](https://github.com/transmission-remote-gui/transgui/issues/924) (File Manager & Shortcuts), [#1020](https://github.com/transmission-remote-gui/transgui/issues/1020) (User-Defined Menu *Windows Only*) and [#1070](https://github.com/transmission-remote-gui/transgui/issues/1070) (.torrent Auto Opening)

```ini
[Interface]
; Maximum number of elements in the folder history list
MaxFoldersHistory=10

[Interface]
; If "Open Container Folder" gives you an error, try the nondefault value for your platform
; Linux: FileOpenDoc=0 (default 1)
; macOS: FileOpenDoc=1 (default 0)

[Interface]
; Alternate File Manager (Windows Only)
FileManagerDefault={Full path to your File Manager .exe}
FileManagerDefaultParam={Alternate parameters (may be left blank)}

[Interface]
; System-wide Shortcut key (Windows Only)
; Full list: https://docwiki.embarcadero.com/RADStudio/Seattle/en/Virtual_Key_Codes (Plus VK_A...VK_Z and VK_0..VK_9)
GlobalHotkey={Virtual Key Code}
; MOD_SHIFT, MOD_CONTROL, MOD_ALT, MOD_WIN alone or combined with a + sign
GlobalHotkeyMod={Modifier Key}

[Interface]
WatchLocalFolder={LOCAL Folder to watch for torrent files}
WatchDestinationFolder={REMOTE destination where the data would be saved; if this value is missing or empty, the last destination folder is used}
; Time period in MINUTES between folder scans for torrents
; Fractional values are allowed; use 0.5 or 0,5 for 30 seconds, depending on locale
WatchInterval=1

[Shortcuts]
; Modify all the shortcuts of the GUI here

[Usermenu]
Caption1={Caption in the menu}
ExeName1={Full path to the program .exe you want to add to the menu}
; Usually "%s" but some programs may require additional parameters
Params1="%s"
Caption2={Same for item 2}
ExeName2={Same for item 2}
Params2={Same for item 2}

[StatusBarPanels]
; Customize the width of the status bar panels to fit your language
; 0 is the leftmost panel and 7 is the rightmost panel.
0=327
1=152
2=152
3=130
4=130
5=130
6=130
7=130

[MainForm]
; Size of the Big Icon Toolbar buttons (defaults to 64 if missing)
BigToolBarHeight=48

[MainForm]
; 1=Shows dates relative to now, 0=Shows absolute dates using the system date and time format
FromNow=1
```

## License

Copyright (c) 2008-2019 by Yury Sidorov and the Transmission Remote GUI working group.

Transmission Remote GUI is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

Transmission Remote GUI is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
