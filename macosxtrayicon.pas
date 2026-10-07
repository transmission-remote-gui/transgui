{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  In addition, as a special exception, the copyright holders give permission to
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}
unit macosxtrayicon;
{$mode objfpc}{$H+}
{$ifdef darwin}
{$modeswitch objectivec1}
{$endif darwin}

interface

{$ifdef darwin}
uses
  Graphics;

type
  TAppActivateEvent = procedure of object;

// Loads the monochrome menu bar icon (menubar.png) into AIcon.
// Returns False when the icon file was not found.
function LoadMenuBarIcon(AIcon: TIcon): Boolean;

// Marks the Cocoa image behind AIcon as a template image, so macOS paints it
// with the menu bar appearance (dark/light) instead of its original colors.
procedure MakeTemplateIcon(AIcon: TIcon);

// Calls AHandler each time the application is activated (Dock icon click,
// open -a, app switching) and when the system asks the running application to
// reopen (open -a, Spotlight, Finder, AppleScript "activate"). LCL's own
// OnActivate cannot be used for this because it stops working once the
// application has no visible windows.
procedure RegisterAppActivateHandler(AHandler: TAppActivateEvent);
procedure UnregisterAppActivateHandler;

// Tells the unit that the main form is fully created. Reopen and activation
// events sent together with the application launch are ignored until then:
// handling them during the startup breaks the main window creation.
procedure NotifyAppReady;
{$endif darwin}

implementation

{$ifdef darwin}
uses
  SysUtils, Classes, MacOSAll, CocoaAll, Cocoa_Extra, cocoagdiobjects;

type
  TAppActivateObserver = objcclass(NSObject)
  public
    procedure lclAppBecameActive(notification: NSNotification); message 'lclAppBecameActive:';
  end;

  // Handles the kAEOpenApplication Apple event, which LaunchServices sends when
  // the application is opened through Finder, Spotlight or "open -a". The event
  // may be delivered while the application is still starting up, so the work is
  // postponed to the next run loop pass. Regular reopen requests reach the
  // application as a plain activation and are covered by TAppActivateObserver.
  TAppOpenObserver = objcclass(NSObject)
  public
    procedure handleOpenEvent(event: NSAppleEventDescriptor; replyEvent: NSAppleEventDescriptor);
      message 'handleAppleEvent:withReplyEvent:';
    procedure deferredOpen(dummy: id);
      message 'deferredOpen:';
  end;

var
  AppReady: Boolean = False;
  AppActivateHandler: TAppActivateEvent = nil;
  AppActivateObserver: TAppActivateObserver = nil;
  AppOpenObserver: TAppOpenObserver = nil;

procedure TAppActivateObserver.lclAppBecameActive(notification: NSNotification);
begin
  if not AppReady then exit;
  if Assigned(AppActivateHandler) then
    AppActivateHandler();
end;

procedure TAppOpenObserver.handleOpenEvent(event: NSAppleEventDescriptor; replyEvent: NSAppleEventDescriptor);
begin
  if not AppReady then exit;
  // The reopen event is delivered while the application launch may still be in
  // progress: activating the application synchronously inside the handler
  // prevents the main window from being shown afterwards. Postpone the work to
  // the next run loop pass, when Application.Run has already started.
  performSelector_withObject_afterDelay(ObjCSelector('deferredOpen:'), nil, 0.5);
end;

procedure TAppOpenObserver.deferredOpen(dummy: id);
begin
  if Assigned(AppActivateHandler) then
    AppActivateHandler();
  // Replaces the default handler of NSApplication: activate the application so
  // its window becomes the key one even when it is not the frontmost process.
  {$ifdef BOOLFIX}
  NSApp.activateIgnoringOtherApps_(Ord(True));
  {$else}
  NSApp.activateIgnoringOtherApps(True);
  {$endif}
end;

const
  MenuBarIconName = 'menubar.png';

function FindMenuBarIcon: string;
var
  ExeDir: string;
begin
  Result:='';
  ExeDir:=ExtractFilePath(ParamStr(0));
  if FileExists(ExeDir + '../Resources/' + MenuBarIconName) then
    // application bundle: Contents/MacOS/transgui -> Contents/Resources/menubar.png
    Result:=ExeDir + '../Resources/' + MenuBarIconName
  else if FileExists(ExeDir + 'setup/macosx/' + MenuBarIconName) then
    // development build: transgui -> setup/macosx/menubar.png
    Result:=ExeDir + 'setup/macosx/' + MenuBarIconName
  else if FileExists(ExeDir + MenuBarIconName) then
    Result:=ExeDir + MenuBarIconName;
end;

function LoadMenuBarIcon(AIcon: TIcon): Boolean;
var
  Pic: TPicture;
  FileName: string;
begin
  Result:=False;
  if AIcon = nil then exit;
  FileName:=FindMenuBarIcon;
  if FileName = '' then exit;
  Pic:=TPicture.Create;
  try
    try
      Pic.LoadFromFile(FileName);
    except
      exit;
    end;
    if Pic.Graphic = nil then exit;
    // TIcon.LoadFromFile cannot read PNGs, so the picture is assigned instead.
    AIcon.Assign(Pic.Graphic);
    Result:=not AIcon.Empty;
  finally
    Pic.Free;
  end;
end;

procedure MakeTemplateIcon(AIcon: TIcon);
var
  Bitmap: TCocoaBitmap;
begin
  if (AIcon = nil) or (AIcon.Handle = 0) then exit;
  Bitmap:=TCocoaBitmap(AIcon.Handle);
  if (Bitmap = nil) or (Bitmap.Image = nil) then exit;
  Bitmap.Image.setTemplate(True);
end;

procedure RegisterAppActivateHandler(AHandler: TAppActivateEvent);
begin
  AppActivateHandler:=AHandler;
  if AppActivateObserver = nil then begin
    AppActivateObserver:=TAppActivateObserver.new;
    NSNotificationCenter.defaultCenter.addObserver_selector_name_object(
      AppActivateObserver,
      ObjCSelector('lclAppBecameActive:'),
      NSApplicationDidBecomeActiveNotification, nil);
  end;
  if AppOpenObserver = nil then begin
    AppOpenObserver:=TAppOpenObserver.new;
    NSAppleEventManager.sharedAppleEventManager.setEventHandler_andSelector_forEventClass_andEventID(
      AppOpenObserver, ObjCSelector('handleAppleEvent:withReplyEvent:'),
      kCoreEventClass, kAEOpenApplication);
  end;
end;

procedure NotifyAppReady;
begin
  AppReady:=True;
end;

procedure UnregisterAppActivateHandler;
begin
  AppActivateHandler:=nil;
  if AppOpenObserver <> nil then begin
    NSAppleEventManager.sharedAppleEventManager.removeEventHandlerForEventClass_andEventID(
      kCoreEventClass, kAEOpenApplication);
    AppOpenObserver.release;
    AppOpenObserver:=nil;
  end;
  if AppActivateObserver <> nil then begin
    NSNotificationCenter.defaultCenter.removeObserver(AppActivateObserver);
    AppActivateObserver.release;
    AppActivateObserver:=nil;
  end;
end;
{$endif darwin}

end.
