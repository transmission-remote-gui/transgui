Transmission Remote GUI.
Copyright (c) 2008-2010 by Yury Sidorov.

Transmission Remote GUI is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Transmission Remote GUI is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
*********************************************************************************

Transmission Remote GUI is feature rich cross platform front-end to remotely control Transmission daemon via its RPC protocol. It is faster and has more functionality than build-in Transmission web interface.

Transmission Remote GUI is developed using Lazarus RAD and Free Pascal compiler.

Features:
 * Native application for Windows and Linux (GTK2)
 * uTorrent-like interface
 * Select files to download
 * Choose files priority
 * View details about connected peers
 * Full information about each torrent
 * Per torrent options

Project home:
http://code.google.com/p/transmisson-remote-gui/

INSTALLATION

LINUX:

Easy way (recommended).

There are precompiled program's binaries for i386 and x86_64 Linux architectures.
- Download a .zip archive for your architecture.
- Unzip it to your home dir.
- Create a desktop or menu shortcut to the transgui executable.
  * (If needed change the transgui file permissions to executable).
- Run the program using the created shortcut.

Harder way.

Build the program by yourself.
- Make sure you have working Lazarus and Free Pascal compiler installed.
  * Free Pascal Compiler 2.4 and Lazarus 0.9.28.2 is used to develop Transmission Remote GUI. You may use different versions of FPC and Lazarus at your own risk.
- Download sources archive and extract it to some folder or perform svn checkout.
- Run Lazarus IDE, open transgui project and build it.

Or to build from command line:
 * Open terminal/command line prompt and cd to the sources folder;
 * Execute lazbuild transgui.lpi command to build the application. 

More info here:
http://code.google.com/p/transmisson-remote-gui/wiki/Building
