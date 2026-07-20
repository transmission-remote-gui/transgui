Transmission Remote GUI.
Copyright (c) 2008-2019 by Yury Sidorov and the Transmission Remote GUI working group.

Transmission Remote GUI is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Transmission Remote GUI is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

In addition, as a special exception, OpenVPN Technologies, Inc. gives
permission to link the code of this program with the OpenSSL Library (or with
modified versions of OpenSSL that use the same license as OpenSSL), and
distribute linked combinations including the two. You must obey the GNU General
Public License in all respects for all of the code used other than OpenSSL. If
you modify this file, you may extend this exception to your version of the
file, but you are not obligated to do so. If you do not wish to do so, delete
this exception statement from your version.
*********************************************************************************

Transmission Remote GUI is a feature-rich, cross-platform front-end to remotely control a Transmission daemon via its RPC protocol. It is faster and has more functionality than the built-in Transmission web interface.

Transmission Remote GUI is developed using Lazarus RAD and the Free Pascal compiler.

Features:
 * Native application for Windows, Linux and macOS
 * uTorrent-like interface
 * Select files to download
 * Choose file priority
 * View details about connected peers
 * Full information about each torrent
 * Per-torrent options

Project home:
https://github.com/transmission-remote-gui/transgui

INSTALLATION

The installers are listed on the GitHub Releases page:
https://github.com/transmission-remote-gui/transgui/releases/latest

Installation instructions for different platforms:

LINUX:

Easy way (recommended).

Precompiled binaries for supported Linux architectures are available on the Releases page.
- Download and extract the release for your architecture.
- Run the transgui executable directly or using a desktop or menu shortcut.
  * (Make the transgui file executable if needed.)

Harder way.

Build the program by yourself.
- Make sure you have Lazarus and the Free Pascal compiler installed and working.
- Download the source archive and extract it to a folder, or clone the repository.
- Open a terminal/command-line prompt and cd to the source folder;
- Execute the "lazbuild -B transgui.lpi --lazarusdir=<lazarus_dir>" command to build the transgui.res file, replacing <lazarus_dir> with your Lazarus installation path;
- Execute the "make LAZARUS_DIR=<lazarus_dir>" command to build the application;
- Execute the "make LAZARUS_DIR=<lazarus_dir> zipdist" command to create a release .zip archive in the "Release" sub-folder.

WINDOWS:

Portable zip archive (recommended).
- The zip archive release is much smaller than the installer one, which can save you some bandwidth, disk space and time. Simply download and extract the zip archive wherever you want, then run "transgui.exe" directly or create a shortcut to it.
- If you need SSL/TLS support with a portable release, download the source archive for the tag matching your release:
  https://github.com/transmission-remote-gui/transgui/tags
- Copy the project's OpenSSL DLLs into the same folder as "transgui.exe":
  - 32-bit: setup/win/openssl
  - 64-bit: setup/win_amd64/openssl
- If Windows reports that MSVCR120.dll is missing, install the Microsoft Visual C++ 2013 Redistributable; if VCRUNTIME140.dll is missing, install the latest v14 Redistributable. Download the package matching your build architecture from Microsoft's supported Visual C++ Redistributable downloads:
  https://learn.microsoft.com/cpp/windows/latest-supported-vc-redist
- Do not mix files from different releases or architectures.

Installer.
- This installer has an additional installation wizard and bundles the OpenSSL DLLs in every release; its size is much larger than the zip archive, but you don't need to take care of the OpenSSL dependencies.
  1. Directly download the installer.
  2. Run the installer and follow the steps to install it on your system.

Using Chocolatey.
- Run "choco install transgui" to install the latest version of Transmission Remote GUI.

MACOS:

Without a package manager.
This method requires no additional prerequisites or dependencies:
  1. Download the disk image from the release page.
  2. Open the image file to mount the image.
  3. Directly run the application or drag the app icon to your disk / Applications folder.

Using Homebrew.
- You need to have Homebrew installed. Execute this command to install Transmission Remote GUI: "brew install --cask transmission-remote-gui"

COMMAND LINE PARAMETERS

You can specify a path to a .torrent file or a magnet link as a command-line parameter. The program will add the specified torrent.

-hidden: Start the program hidden. Only the program's tray icon will be visible.
--home=<home_dir>: Specifies a home directory for the program. All the program's settings are stored in the home directory. You can run multiple instances of the program by specifying different home directories. This option takes precedence over portable mode and the default user-profile location.

PORTABLE MODE

If the program finds the transgui.ini file in the same folder as the binary file, then it will store all configuration and data files in the program's folder, instead of the folder in a user profile.

FIXED SHORTCUTS

Alt + 1: All Torrents
Alt + 2: Downloading
Alt + 3: Completed
Alt + 4: Active
Alt + 5: Inactive
Alt + 6: Stopped
Alt + 7: Error
Alt + 8: Waiting
Alt + S: Search box (filter torrents by keywords) - Esc cancels the filter and clears the box.
Alt + G: Info Pane - General Tab
Alt + K: Info Pane - Trackers Tab
Alt + P: Info Pane - Peers Tab
Alt + F: Info Pane - Files Tab


ADVANCED PARAMETERS

There are some parameters in the transgui.ini file that cannot be modified via the GUI.
More info on: https://github.com/transmission-remote-gui/transgui/issues/924 (File Manager & Shortcuts),
https://github.com/transmission-remote-gui/transgui/issues/1020 (User-Defined Menu -Windows Only-)
and https://github.com/transmission-remote-gui/transgui/issues/1070 (.torrent Auto Opening)

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
*********************************************************************************
Big Icons

Farm-Fresh Fatcow Web Hosting
http://www.fatcow.com/
License Creative Commons (Attribution 3.0 United States)
https://creativecommons.org/licenses/by/3.0/us/legalcode

Visual Farm
http://icons8.com/
License Creative Commons Attribution-No Derivative Works 3.0 Unported
https://creativecommons.org/licenses/by-nd/3.0/legalcode
