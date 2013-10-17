{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2012 by Yury Sidorov.

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
  SysUtils, Classes, Controls, Forms, IniFiles,
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

  { TIniFileUtf8 }

  TIniFileUtf8 = class(TIniFile)
  private
    FStream: TFileStreamUTF8;
    FFileName: string;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    destructor Destroy; override;
    procedure UpdateFile; override;
  end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle;
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;

function GetTimeZoneDelta: TDateTime;

procedure ShowTaskbarButton;
procedure HideTaskbarButton;
function IsTaskbarButtonVisible: boolean;

procedure CenterOnParent(C: TControl);

function OpenURL(const URL: string; const Params: string = ''): boolean;

function CompareFilePath(const p1, p2: string): integer;

procedure AppBusy;
procedure AppNormal;
procedure ForceAppNormal;

function ParamStrUTF8(Param: Integer): utf8string;
function ParamCount: integer;
function GetCmdSwitchValue(const Switch: string): string;

function Base32Decode(const s: ansistring): ansistring;

{$ifdef CALLSTACK}
function GetLastExceptionCallStack: string;
{$endif CALLSTACK}

{$ifdef mswindows}
procedure AllowSetForegroundWindow(dwProcessId: DWORD);
{$endif mswindows}

implementation

uses
{$ifdef CALLSTACK}
  lineinfo2,
{$endif CALLSTACK}
  FileUtil;

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

  if FParams = nil then begin
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
    // Getting real executable name
    SetLength(s, 1000);
    SetLength(s, GetModuleFileNameW(HINSTANCE, PWideChar(s), Length(s) + 1));
    if s <> '' then
      if FParams.Count > 0 then
        FParams[0]:=UTF8Encode(s)
      else
        FParams.Add(UTF8Encode(s));
  end;

  if Param >= FParams.Count then
    Result:=''
  else
    Result:=FParams[Param];
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

{ TIniFileUtf8 }

constructor TIniFileUtf8.Create(const AFileName: string; AEscapeLineFeeds: Boolean);
var
  m: integer;
begin
  FFileName:=AFileName;
  if FileExistsUTF8(FFileName) then
    m:=fmOpenRead or fmShareDenyNone
  else
    m:=fmCreate;
  FStream:=TFileStreamUTF8.Create(AFileName, m);
  inherited Create(FStream, AEscapeLineFeeds);
  FileClose(FStream.Handle);
  THandle(pointer(@FStream.Handle)^):=0;
end;

destructor TIniFileUtf8.Destroy;
begin
  inherited Destroy;
  FStream.Free;
end;

procedure TIniFileUtf8.UpdateFile;
var
  h: THANDLE;
begin
  if FileExistsUTF8(FFileName) then
    h:=FileOpenUTF8(FFileName, fmOpenWrite or fmShareDenyWrite)
  else
    h:=FileCreateUTF8(FFileName);
  if h = THandle(-1) then
    raise Exception.Create('Unable to write to INI file.' + LineEnding + SysErrorMessageUTF8(GetLastOSError));
  THandle(pointer(@FStream.Handle)^):=h;
  try
    inherited UpdateFile;
    FStream.Size:=FStream.Position;
  finally
    FileClose(FStream.Handle);
    THandle(pointer(@FStream.Handle)^):=0;
  end;
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

function IsTaskbarButtonVisible: boolean;
begin
{$ifdef mswindows}
  Result:=IsWindowVisible(TWin32WidgetSet(WidgetSet).AppHandle);
{$else}
  Result:=Application.MainForm.Visible;
{$endif mswindows}
end;

{$ifdef unix}
function UnixOpenURL(const FileName: String):Integer;
var
  WrkProcess: TProcess;
  cmd, fn: String;
begin
  Result:=-1;
  cmd:=FindDefaultExecutablePath('xdg-open');
  if cmd = '' then begin
    cmd:=FindDefaultExecutablePath('gnome-open');
    if cmd = '' then begin
      cmd:=FindDefaultExecutablePath('kioclient');
      if cmd <> '' then
        cmd:=cmd + ' exec'
      else begin
        cmd:=FindDefaultExecutablePath('kfmclient');
        if cmd = '' then
          exit;
        cmd:=cmd + ' exec';
      end;
    end;
  end;

  fn:=FileName;
  if Pos('://', fn) > 0 then
    fn:=StringReplace(fn, '#', '%23', [rfReplaceAll]);

  WrkProcess:=TProcess.Create(nil);
  try
    WrkProcess.Options:=[poNoConsole];
    WrkProcess.CommandLine:=cmd + ' "' + fn + '"';
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
  s, p: widestring;
{$endif mswindows}
begin
{$ifdef mswindows}
  s:=UTF8Decode(URL);
  p:=UTF8Decode(Params);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:=ShellExecuteW(0, 'open', PWideChar(s), PWideChar(p), nil, SW_SHOWNORMAL) > 32
  else
    Result:=ShellExecuteA(0, 'open', PChar(ansistring(s)), PChar(ansistring(p)), nil, SW_SHOWNORMAL) > 32;
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

function GetCmdSwitchValue(const Switch: string): string;
var
  i, len: integer;
  s, ss: string;
begin
  Result:='';
  ss:='--' + Switch + '=';
  len:=Length(ss);
  for i:=1 to ParamCount do begin
    s:=ParamStrUTF8(i);
    if Copy(s, 1, len) = ss then begin
      Result:=Copy(s, len + 1, MaxInt);
      break;
    end;
  end;
end;

{$ifdef CALLSTACK}
function GetLastExceptionCallStack: string;

  function GetAddrInfo(addr: pointer): string;
  var
    func,
    source : shortstring;
    line   : longint;
  begin
    GetLineInfo(ptruint(addr), func, source, line);
    Result:='$' + HexStr(ptruint(addr), sizeof(ptruint) * 2);
    if func<>'' then
      Result:=Result + '  ' + func;
    if source<>'' then begin
      if func<>'' then
        Result:=Result + ', ';
      if line<>0 then
        Result:=Result + ' line ' + IntToStr(line);
      Result:=Result + ' of ' + source;
    end;
  end;

var
  I: Integer;
  Frames: PPointer;
begin
  Result := GetAddrInfo(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + LineEnding + GetAddrInfo(Frames[I]);
end;
{$endif CALLSTACK}

procedure CenterOnParent(C: TControl);
var
  R: TRect;
begin
  R:=C.BoundsRect;
  R.Left:=(C.Parent.ClientWidth - C.Width) div 2;
  R.Top:=(C.Parent.ClientHeight - C.Height) div 2;
  C.BoundsRect:=R;
end;

{$PUSH}
{$RANGECHECKS OFF}
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
{$POP}

finalization
{$ifdef windows}
  FreeAndNil(FParams);
{$endif windows}

end.

