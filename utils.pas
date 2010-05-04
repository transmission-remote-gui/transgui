{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2009 by Yury Sidorov.

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
*************************************************************************************}

unit utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Controls, Forms,
{$ifdef windows}
  Windows, win32int, InterfaceBase
{$endif}
{$ifdef unix}
  baseunix, unix, unixutil, process
{$endif}
  ;

function GetTimeZoneDelta: TDateTime;

procedure ShowTaskbarButton;
procedure HideTaskbarButton;

function OpenURL(const URL: string): boolean;

procedure AppBusy;
procedure AppNormal;
procedure ForceAppNormal;

implementation

function GetTimeZoneDelta: TDateTime;
{$ifdef windows}
var
  t: TIME_ZONE_INFORMATION;
  res: dword;
{$endif}
begin
  Result:=0;
{$ifdef windows}
  res:=GetTimeZoneInformation(t);
  if res<> TIME_ZONE_ID_INVALID then begin
    case res of
      TIME_ZONE_ID_STANDARD:
        Result:=-t.StandardBias;
      TIME_ZONE_ID_DAYLIGHT:
        Result:=-t.DaylightBias;
    end;
    Result:=(-t.Bias + Result)/MinsPerDay;
  end;
{$endif}
{$ifdef unix}
  Result:=Tzseconds/SecsPerDay;
{$endif}
end;

procedure ShowTaskbarButton;
begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_SHOW);
{$endif mswindows}
end;

procedure HideTaskbarButton;
begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_HIDE);
{$endif mswindows}
end;

{$ifdef unix}
type
  TipoDeskMang = (dmUnknown, dmGnome, dmKde);

function  DeterminaDesktopManager:TipoDeskMang;
begin
  Result:=dmUnknown; //Desktop Manager sconosciuto

  if (GetEnvironmentVariable('GNOME_DESKTOP_SESSION_ID') <> '') or
     (GetEnvironmentVariable('DESKTOP_SESSION') = 'gnome') then begin
    Result:=dmGnome;
  end else begin
    if (GetEnvironmentVariable('KDE_FULL_SESSION') <> '') or
       (GetEnvironmentVariable('DESKTOP_SESSION') = 'kde') then begin
      Result:=dmKde;
    end;
  end;
end;

function UnixOpenURL(const NomeFile: String):Integer;
var
  WrkProcess:TProcess;
  Comando:String;
begin
  Result:=-1;
  Comando:='';
  case DeterminaDesktopManager of
    dmGnome:
      Comando:='gnome-open';
    dmKde:
      Comando:='kfmclient exec';
  end;

  if Comando = '' then
    exit;

  WrkProcess:=TProcess.Create(nil);
  try
    WrkProcess.Options:=[poNoConsole];
    WrkProcess.CommandLine:=Comando +
                            ' ' +
                            NomeFile;
    WrkProcess.Execute;
    Result:=WrkProcess.ExitStatus;
  finally
    WrkProcess.Free;
  end;
end;
{$endif unix}

function OpenURL(const URL: string): boolean;
{$ifdef mswindows}
var
  s: string;
{$endif mswindows}
begin
{$ifdef mswindows}
  s:=UTF8Decode(URL);
  Result:=ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOWNORMAL) > 32;
{$endif mswindows}

{$ifdef darwin}
  Result:=fpSystem('Open ' + URL) = 0;
{$else darwin}

  {$ifdef unix}
    Result:=UnixOpenURL(URL) = 0;
  {$endif unix}

{$endif darwin}
end;

var
  BusyCount: integer = 0;

procedure AppBusy;
begin
  Inc(BusyCount);
  Screen.Cursor:=crHourGlass;
end;

procedure AppNormal;
begin
  Dec(BusyCount);
  if BusyCount <= 0 then begin
    BusyCount:=0;
    Screen.Cursor:=crDefault;
  end;
end;

procedure ForceAppNormal;
begin
  BusyCount:=0;
  AppNormal;
end;

end.

