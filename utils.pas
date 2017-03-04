{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2010 by Yury Sidorov.

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
  SysUtils, Classes, Controls, Forms,
{$ifdef windows}
  Windows, win32int, InterfaceBase
{$endif}
{$ifdef unix}
  baseunix, unix, unixutil, process
{$endif}
  ;

type

  { TFileStreamUTF8 }

  TFileStreamUTF8 = class(THandleStream)
  private
    FFileName: utf8string;
  public
    constructor Create(const AFileName: utf8string; Mode: Word);
    constructor Create(const AFileName: utf8string; Mode: Word; Rights: Cardinal);
    destructor Destroy; override;
    property FileName: utf8string Read FFilename;
  end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle;
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;

function GetTimeZoneDelta: TDateTime;

procedure ShowTaskbarButton;
procedure HideTaskbarButton;

function OpenURL(const URL: string; const Params: string = ''): boolean;

function CompareFilePath(const p1, p2: string): integer;

procedure AppBusy;
procedure AppNormal;
procedure ForceAppNormal;

function ParamStrUTF8(Param: Integer): utf8string;
function ParamCount: integer;

function Base32Decode(const s: ansistring): ansistring;

{$ifdef mswindows}
procedure AllowSetForegroundWindow(dwProcessId: DWORD);
{$endif mswindows}

implementation

uses FileUtil;

{$ifdef windows}
function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
const
  AccessMode: array[0..2] of Cardinal  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), dword(AccessMode[Mode and 3]),
                       dword(ShareMode[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

var
  FParams: TStringList;

function ParamStrUTF8(Param: Integer): utf8string;

  function SkipSpaces( P: PWideChar ): PWideChar;
  begin
    while True do
    begin
      while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
      if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
    end;
    Result := P;
  end;

  function SkipParam(P: PWideChar): PWideChar;
  begin
    P := SkipSpaces( P );
    while P[0] > ' ' do
      if P[0] = '"' then
      begin
        Inc(P);
        while (P[0] <> #0) and (P[0] <> '"') do
          Inc(P);
        if P[0] <> #0 then Inc(P);
      end
        else
        Inc(P);
    Result := P;
  end;

var
  P, P1: PWideChar;
  s: widestring;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result:=FileUtil.ParamStrUTF8(Param);
    exit;
  end;

  if FParams <> nil then begin
    if Param >= FParams.Count then
      Result:=''
    else
      Result:=FParams[Param];
    exit;
  end;

  FParams:=TStringList.Create;
  P := GetCommandLineW;
  while True do begin
    P := SkipSpaces( P );
    P1 := P;
    P := SkipParam(P);
    if P = P1 then
      break;
    s := Copy( P1, 1, P - P1 );
    if Length(s) >= 2 then
      if (s[1] = '"') and (s[Length(s)] = '"') then
        s:=Copy(s, 2, Length(s) - 2);
    FParams.Add(UTF8Encode(s));
  end;
end;

function ParamCount: integer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Result:=System.ParamCount
  else begin
    if FParams = nil then
      ParamStrUTF8(0);
    Result:=FParams.Count - 1;
  end;
end;

{$else} // Non-Windows targets

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  Result:=FileOpen(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result:=FileCreate(FileName);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result:=FileCreate(FileName, Rights);
end;

function ParamStrUTF8(Param: Integer): utf8string;
begin
  Result:=FileUtil.ParamStrUTF8(Param);
end;

function ParamCount: integer;
begin
  Result:=System.ParamCount;
end;

{$endif windows}

{ TFileStreamUTF8 }

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: Word);
var
  lHandle: THandle;
begin
  FFileName:= AFileName;
  if Mode = fmcreate then
    lHandle:= FileCreateUTF8(AFileName)
  else
    lHandle:= FileOpenUTF8(AFileName, Mode);

  If (THandle(lHandle)=feInvalidHandle) then
  begin
    if Mode = fmCreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"', [AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [AFilename]);
  end
  else
    inherited Create(lHandle);
end;

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: Word; Rights: Cardinal);
var
  lHandle: THandle;
begin
  FFileName:=AFileName;
  if Mode=fmcreate then
    lHandle:=FileCreateUTF8(AFileName,Rights)
  else
    lHandle:=FileOpenUTF8(AFileName,Mode);

  if (THandle(lHandle)=feInvalidHandle) then
  begin
    if Mode=fmcreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"',[AFilename]);
  end
  else
    inherited Create(lHandle);
end;

destructor TFileStreamUTF8.Destroy;
begin
  FileClose(Handle);
end;

// ---------------------------------------------

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
{$else}
  Application.MainForm.Visible:=True;
{$endif mswindows}
end;

procedure HideTaskbarButton;
begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_HIDE);
{$else}
  Application.MainForm.Visible:=False;
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
  WrkProcess: TProcess;
  Comando, fn: String;
begin
  Result:=-1;
  Comando:='';
  case DeterminaDesktopManager of
    dmGnome:
      Comando:='gnome-open';
    dmKde:
      if FileExists('/usr/bin/kioclient') then
        Comando:='/usr/bin/kioclient exec'
      else
        Comando:='kfmclient exec';
  end;

  if Comando = '' then
    exit;

  fn:=NomeFile;
  if Pos('://', fn) > 0 then
    fn:=StringReplace(fn, '#', '%23', [rfReplaceAll]);

  WrkProcess:=TProcess.Create(nil);
  try
    WrkProcess.Options:=[poNoConsole];
    WrkProcess.CommandLine:=Comando + ' "' + fn + '"';
    WrkProcess.Execute;
    Result:=WrkProcess.ExitStatus;
  finally
    WrkProcess.Free;
  end;
end;
{$endif unix}

function OpenURL(const URL, Params: string): boolean;
{$ifdef mswindows}
var
  s, p: string;
{$endif mswindows}
begin
{$ifdef mswindows}
  s:=UTF8Decode(URL);
  p:=UTF8Decode(Params);
  Result:=ShellExecute(0, 'open', PChar(s), PChar(p), nil, SW_SHOWNORMAL) > 32;
{$endif mswindows}

{$ifdef darwin}
  Result:=fpSystem('Open "' + URL + '"') = 0;
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

{$ifdef mswindows}
procedure AllowSetForegroundWindow(dwProcessId: DWORD);
type
  TAllowSetForegroundWindow = function(dwProcessId: DWORD): BOOL; stdcall;
var
  _AllowSetForegroundWindow: TAllowSetForegroundWindow;
begin
  _AllowSetForegroundWindow:=TAllowSetForegroundWindow(GetProcAddress(GetModuleHandle('user32.dll'), 'AllowSetForegroundWindow'));
  if Assigned(_AllowSetForegroundWindow) then
    _AllowSetForegroundWindow(dwProcessId);
end;
{$endif mswindows}

function CompareFilePath(const p1, p2: string): integer;
begin
{$ifdef windows}
  Result:=AnsiCompareText(UTF8Decode(p1), UTF8Decode(p2));
{$else}
Result:=AnsiCompareStr(UTF8Decode(p1), UTF8Decode(p2));
{$endif windows}
end;

function Base32Decode(const s: ansistring): ansistring;
var
  optr, len, bcnt: integer;
  Output: PByteArray;
  Input: PAnsiChar;
  w: word;
  c: ansichar;
  b: byte;
begin
  len:=Length(s);
  Input:=PAnsiChar(s);
  SetLength(Result, len);
  Output:=PByteArray(PAnsiChar(Result));
  optr:=0;
  w:=0;
  bcnt:=0;
  while len > 0 do begin
    repeat
      c:=Input^;
      if c = '=' then begin
        len:=0;
        break;
      end;
      if c in ['A'..'Z'] then
        b:=Ord(c) - Ord('A')
      else
        if c in ['2'..'7'] then
          b:=Ord(c) - 24
        else
          raise Exception.Create('Invalid base32 string.');
      w:=(w shl 5) or b;
      Inc(bcnt, 5);
      Inc(Input);
      Dec(len);
    until (bcnt >= 8) or (len <= 0);
    if bcnt < 8 then
      Output^[optr]:=w shl (8 - bcnt)
    else begin
      Dec(bcnt, 8);
      Output^[optr]:=w shr bcnt;
    end;
    Inc(optr);
  end;
  SetLength(Result, optr);
end;

end.

