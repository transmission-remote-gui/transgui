{
    This file is part of the Free Pascal Free Component Library.

    Unicode name file access and utility functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    The unit is based on FPC RTL SysUtils

 **********************************************************************}
unit
  ufiles;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, sysconst, RtlConsts, ufilescore
  {$ifdef MSWindows}
  //Win32 implementations
  ,ufileswin
  {$endif}
  {$ifdef darwin}
  // MacOSX implementations
  ,ufilesmac
  {$endif}
  {$ifdef linux}
  // Linux implementations
  ,ufileslinux
  {$endif}
  ;

type
  TSearchRecU = ufilescore.TSearchRecUInt;

//
function DeleteFile (const FileName : UnicodeString): Boolean;
function RenameFile (const OldName, NewName : UnicodeString): Boolean;
function FindFirst (const Path : UnicodeString; Attr : Longint; out Rslt : TSearchRecU) : Longint;
function FindNext (var Rslt : TSearchRecU) : Longint;
procedure FindClose (var F : TSearchrecU);

function FileGetAttr (const FileName : UnicodeString) : DWORD;
function FileSetAttr (const Filename : UnicodeString; Attr: DWORD) : Longint;

function FileOpen (const FileName : UnicodeString; Mode : Integer) : THandle;
function FileCreate (const FileName : UnicodeString) : THandle; overload;
function FileCreate (const FileName : UnicodeString; Mode : Integer) : THandle; overload;

//
function FileIsReadOnly(const FileName: UnicodeString): Boolean;
function FileExists (const FileName : UnicodeString) : Boolean;
function DirectoryExists (const Directory : UnicodeString) : Boolean;

//
function ChangeFileExt(const FileName, Extension: UnicodeString): UnicodeString;
function ExtractFilePath(const FileName: UnicodeString): UnicodeString;
function ExtractFileDrive(const FileName: Unicodestring): UnicodeString;
function ExtractFileName(const FileName: Unicodestring): UnicodeString;
function ExtractFileExt(const FileName: Unicodestring): UnicodeString;
function ExtractFileDir(Const FileName : Unicodestring): UnicodeString;
function ExtractRelativepath (Const BaseName,DestNAme : UnicodeString): UnicodeString;
function IncludeTrailingPathDelimiter(Const Path : UnicodeString) : UnicodeString;
function IncludeTrailingBackslash(Const Path : UnicodeString) : UnicodeString;
function ExcludeTrailingBackslash(Const Path: Unicodestring): UnicodeString;
function ExcludeTrailingPathDelimiter(Const Path: Unicodestring): UnicodeString;
function IsPathDelimiter(Const Path: Unicodestring; Index: Integer): Boolean;
procedure DoDirSeparators (var FileName : UnicodeString);
function SetDirSeparators (const FileName : UnicodeString) : UnicodeString;
function GetDirs (var DirName : UnicodeString; Var Dirs : array of PUnicodeChar) : Longint;

function SameFileName (const S1, S2: UnicodeString): Boolean;
function SameFileName (const S1, S2: UnicodeString; CaseSensitive: Boolean): Boolean;
function CompareFileName (const S1, S2: UnicodeString): Integer;
function CompareFileName (const S1, S2: UnicodeString; CaseSensitive: Boolean): Integer;

function isPathCaseSensitive(const APath: UnicodeString; var CaseSensitive: Boolean): Boolean;

//
function ForceDirectories(const Dir: UnicodeString): Boolean;

function CreateDir(const Dir: UnicodeString): Boolean;
function RemoveDir(const Dir: UnicodeString): Boolean;
function SetCurrentDir(const Dir: UnicodeString): Boolean;
function GetCurrentDir: UnicodeString;

type

  { TFileStreamU }

  TFileStreamU = class(THandleStream)
  Private
    FFileName : UnicodeString;
  public
    constructor Create(const AFileName: UnicodeString; Mode: Word); overload;
    constructor Create(const AFileName: UnicodeString; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    property FileName : UnicodeString Read FFilename;
  end;

implementation

function UCharInSet(const wch: UnicodeChar; const AnsiSet: TAnsiCharSet): Boolean; inline;
begin
  Result := (Word(wch)<=127) and (Char(byte(wch)) in AnsiSet);
end;

function ChangeFileExt(const FileName, Extension: UnicodeString): UnicodeString;
var
  i : longint;
  EndSep : TAnsiCharSet;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not UCharInSet(FileName[I], EndSep) do
    Dec(I);
  if (I = 0) or (FileName[I] <> ExtensionSeparator) then
    I := Length(FileName)+1;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: UnicodeString): UnicodeString;
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (i > 0) and not UCharInSet(FileName[i], EndSep) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;
function ExtractFileDir(Const FileName : Unicodestring): UnicodeString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not UCharInSet(FileName[I], EndSep) do
    Dec(I);
  if (I > 1) and UCharInSet(FileName[I], AllowDirectorySeparators) and
     not (FileName[I - 1] in EndSep) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: Unicodestring): UnicodeString;
var
  i,l: longint;
begin
  Result := '';
  l:=Length(FileName);
  if (l<2) then
    exit;
  If (FileName[2] in AllowDriveSeparators) then
    result:=Copy(FileName,1,2)
  else if UCharInSet(FileName[1], AllowDirectorySeparators) and
          UCharInSet(FileName[2], AllowDirectorySeparators) then
    begin
      i := 2;

      { skip share }
      While (i<l) and Not UCharInSet(Filename[i+1], AllowDirectorySeparators) do
        inc(i);
      inc(i);

      While (i<l) and Not UCharInSet(Filename[i+1], AllowDirectorySeparators) do
        inc(i);
      Result:=Copy(FileName,1,i);
    end;
end;

function ExtractFileName(const FileName: Unicodestring): UnicodeString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not UCharInSet(FileName[I], EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: Unicodestring): UnicodeString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not UCharInSet(FileName[I], EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
    Result := Copy(FileName, I, MaxInt)
  else
    Result := '';
end;

function FileNameCompU(p1, p2: PUnicodeChar): Integer;
var
  l1, l2 : Integer;
  ws1, ws2 : UnicodeString;
begin
  l1:=0;
  while p1[l1]<>#0 do inc(l1);
  l2:=0;
  while p2[l2]<>#0 do inc(l2);
  Result:=l1-l2;
  if Result<>0 then begin
    if Result<0 then Result:=-1
    else Result:=1;
    Exit;
  end;
  if l1=0 then begin
    Result:=0;
    Exit;
  end;
  SetLength(ws1, l1);
  SetLength(ws2, l2);
  Move(p1^, ws1[1], l1*sizeof(UnicodeChar));
  Move(p2^, ws2[1], l1*sizeof(UnicodeChar));
  Result:=CompareFileName(ws1, ws2);
end;

function ExtractRelativepath (Const BaseName,DestName : UnicodeString): UnicodeString;
const
  MaxDirs = 129;
Var
  Source, Dest : UnicodeString;
  Sc,Dc,I,J    : Longint;
  SD,DD        : Array[1..MaxDirs] of PWideChar;
const
  OneLevelBack = '..'+DirectorySeparator;
begin
  If CompareFileName( ExtractFileDrive(BaseName), ExtractFileDrive(DestName)) <> 0 then begin
    Result:=DestName;
    exit;
  end;
  Source:=ExcludeTrailingPathDelimiter(ExtractFilePath(BaseName));
  Dest:=ExcludeTrailingPathDelimiter(ExtractFilePath(DestName));
  SC:=GetDirs (Source,SD);
  DC:=GetDirs (Dest,DD);
  I:=1;
  While (I<=DC) and (I<=SC) do begin
    If FileNameCompU(DD[i],SD[i])=0 then // wrong. can't use PChar function!
      Inc(i)
    else
      Break;
  end;
  Result:='';
  For J:=I to SC do Result:=Result+OneLevelBack;
  For J:=I to DC do Result:=Result+DD[J]+DirectorySeparator;
  Result:=Result+ExtractFileName(DestNAme);
end;

function IncludeTrailingPathDelimiter(Const Path : UnicodeString) : UnicodeString;
Var
  l : Integer;
begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not UCharInSet(Result[l], AllowDirectorySeparators) then
    Result:=Result+DirectorySeparator;
end;

function IncludeTrailingBackslash(Const Path : UnicodeString) : UnicodeString;
begin
  Result:=IncludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingBackslash(Const Path: Unicodestring): UnicodeString;
begin
  Result:=ExcludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingPathDelimiter(Const Path: Unicodestring): UnicodeString;
var
  L : Integer;
begin
  L:=Length(Path);
  If (L>0) and (Path[L] in AllowDirectorySeparators) then
    Dec(L);
  Result:=Copy(Path,1,L);
end;

function IsPathDelimiter(Const Path: Unicodestring; Index: Integer): Boolean;
begin
  Result:=(Index>0) and (Index<=Length(Path)) and UCharInSet(Path[Index], AllowDirectorySeparators);
end;

function SameFileName(const S1, S2: UnicodeString): Boolean;
begin
  Result:=CompareFileName(S1,S2, FileNameCaseSensitive) = 0;
end;

function SameFileName(const S1, S2: UnicodeString; CaseSensitive: Boolean): Boolean;
begin
  Result:=CompareFileName(S1,S2, CaseSensitive)= 0;
end;

function CompareFileName (const S1, S2: UnicodeString): Integer;
begin
  Result:=CompareFileName(s1,s2, FileNameCaseSensitive);
end;

function CompareFileName (const S1, S2: UnicodeString; CaseSensitive: Boolean): Integer;
begin
  Result:=DoCompareFileName(S1, S2, CaseSensitive);
end;

function isPathCaseSensitive(const APath: UnicodeString; var CaseSensitive: Boolean): Boolean;
begin
  Result:=DoPathCaseSensitive(APath, CaseSensitive);
end;

procedure DoDirSeparators (var FileName : UnicodeString);
var
  i : longint;
begin
  for i:=1 to Length(FileName) do
    If UCharInSet(FileName[I], AllowDirectorySeparators) then
      FileName[i]:=DirectorySeparator;
end;

function SetDirSeparators (const FileName : UnicodeString) : UnicodeString;
begin
  Result:=FileName;
  DoDirSeparators (Result);
end;

function GetDirs (var DirName : UnicodeString; Var Dirs : array of PUnicodeChar) : Longint;
Var
  I : Longint;
begin
  I:=1;
  Result:=-1;
  While I<=Length(DirName) do
    begin
    If UCharInSet(DirName[i], AllowDirectorySeparators) and
       { avoid error in case last char=pathdelim }
       (length(dirname)>i) then
      begin
        DirName[i]:=#0;
        Inc(Result);
        Dirs[Result]:=@DirName[I+1];
      end;
    Inc(I);
    end;
  If Result>-1 then inc(Result);
end;


// Ansi version functions.
// The functions are using SysUtils file functions
// File names are converted from Unicode to current system Locale
// Can be used for UTF-8 (modern unixes) OSes, but should not be used for WinNT

function FileGetAttr (const FileName : UnicodeString) : DWord;
begin
  Result:=DoFileGetAttr(FileName);
end;

function FileSetAttr (const Filename : UnicodeString; Attr: DWord) : Longint;
begin
  Result:=DoFileSetAttr(FileName, Attr);
end;

function FileExists (const FileName : UnicodeString) : Boolean;
var
  Attr  : DWord;
begin
  Attr:=FileGetAttr(FileName);
  Result:=Attr<>-1;
  if Result then
    Result:=(Attr and faDirectory)=0;
end;

function DirectoryExists (const Directory : UnicodeString) : Boolean;
var
  Attr  : Integer;
begin
  Attr:=FileGetAttr(Directory);
  Result:=Attr<>-1;
  if Result then
    Result:=(Attr and faDirectory)<>0;
end;


function DeleteFile(const FileName : UnicodeString) : Boolean;
begin
  Result:=DoDeleteFile(FileName);
end;

function RenameFile(const OldName, NewName : UnicodeString) : Boolean;
begin
  Result:=DoRenameFile(OldName, NewName);
end;

function FindFirst (const Path : UnicodeString; Attr : Longint; out Rslt : TSearchRecU) : Longint;
begin
  Result:=DoFindFirst(Path, Attr, Rslt);
end;

function FindNext (var Rslt : TSearchRecU) : Longint;
begin
  Result:=DoFindNext(Rslt);
end;

function FileIsReadOnly(const FileName: UnicodeString): Boolean;
begin
  Result := (FileGetAttr(FileName) and faReadOnly) <> 0;
end;

procedure FindClose (var F : TSearchrecU);
begin
  DoFindClose(f);
end;

function FileOpen (const FileName : UnicodeString; Mode : Integer) : THandle;
begin
 Result:=DoFileOpen(FileName, Mode);
end;

function FileCreate (const FileName : UnicodeString) : THandle;
begin
  Result:=DoFileCreate(FileName, fmCreate);
end;

function FileCreate (const FileName : UnicodeString; Mode : Integer) : THandle;
begin
  Result:=DoFileCreate(FileName, Mode);
end;

function ForceDirectories(const Dir: UnicodeString): Boolean;
var
  E    : EInOutError;
  ADrv : UnicodeString;

  function DoForceDirectories(Const Dir: UnicodeString): Boolean;
  var
    ADir : UnicodeString;
    APath: UnicodeString;
  begin
    Result:=True;
    ADir:=ufiles.ExcludeTrailingPathDelimiter(Dir);
    if (ADir='') then Exit;
    if not ufiles.DirectoryExists(ADir) then
      begin
        APath := ufiles.ExtractFilePath(ADir);
        //this can happen on Windows if user specifies Dir like \user\name/test/
        //and would, if not checked for, cause an infinite recusrsion and a stack overflow
        if (APath = ADir) then Result := False
          else Result:=DoForceDirectories(APath);
        If Result then
          Result := ufiles.CreateDir(ADir);
      end;
  end;

  function IsUncDrive(const Drv: UnicodeString): Boolean;
  begin
    Result := (Length(Drv) > 2) and (Drv[1] = PathDelim) and (Drv[2] = PathDelim);
  end;

begin
  Result := False;
  ADrv := ufiles.ExtractFileDrive(Dir);
  if (ADrv<>'') and (not ufiles.DirectoryExists(ADrv))
  {$IFNDEF FORCEDIR_NO_UNC_SUPPORT} and (not IsUncDrive(ADrv)){$ENDIF} then Exit;
  if Dir='' then
    begin
      E:=EInOutError.Create(SCannotCreateEmptyDir);
      E.ErrorCode:=3;
      Raise E;
    end;
  Result := DoForceDirectories(SetDirSeparators(Dir));
end;

function CreateDir(const Dir: UnicodeString): Boolean;
begin
  Result:=DoCreateDir(Dir);
end;

function RemoveDir(const Dir: UnicodeString): Boolean;
begin
  Result:=DoRemoveDir(Dir);
end;

function SetCurrentDir(const Dir: UnicodeString): Boolean;
begin
  Result:=DoSetCurrentDir(Dir);
end;

function GetCurrentDir: UnicodeString;
begin
  Result:=DoGetCurrentDir();
end;


{ TFileStreamU }

constructor TFileStreamU.Create(const AFileName: UnicodeString; Mode: Word);
var
  hnd : THandle;
begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    hnd:=FileCreate(AFileName)
  else
    hnd:=FileOpen(AFileName,Mode);

  If (THandle(hnd)=feInvalidHandle) then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
  inherited Create(hnd);
end;

constructor TFileStreamU.Create(const AFileName: UnicodeString; Mode: Word; Rights: Cardinal);
var
  hnd : THandle;
begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    hnd:=FileCreate(AFileName,Rights)
  else
    hnd:=FileOpen(AFileName,Mode);

  If (THandle(hnd)=feInvalidHandle) then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
  inherited Create(hnd);
end;

destructor TFileStreamU.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

initialization

finalization

end.
