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
  ufileswin;

interface

{$mode objfpc}{$h+}

uses
  Windows, SysUtils, ufilescore;

implementation

var
  kernel32dll    : THandle = 0;

  FindFirstFileU : function (lpFileName: LPWSTR; var lpFindFileData: TWIN32FindDataW): THandle; stdcall = nil;
  FindNextFileU  : function (hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall = nil;

  SetFileAttributesU : function (lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL; stdcall = nil;
  GetFileAttributesU : function (lpFileName:LPCWSTR):DWORD; stdcall = nil;

  SetCurrentDirectoryU : function (lpPathName:LPCWSTR):WINBOOL; stdcall = nil;
  GetCurrentDirectoryU : function (nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; stdcall = nil;
  CreateDirectoryU : function (lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; stdcall = nil;
  RemoveDirectoryU : function (lpPathName:LPCWSTR): WINBOOL; stdcall = nil;

  CreateFileU : function (lpFileName:LPCWSTR; dwDesiredAccess:DWORD;
    dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES;
    dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;
    hTemplateFile:HANDLE):HANDLE; stdcall = nil;

  DeleteFileU : function (lpFileName:LPCWSTR):WINBOOL; stdcall = nil;
  MoveFileU : function (lpExistingFileName:LPCWSTR;lpNewFileName:LPCWSTR):WINBOOL; stdcall = nil;

  GetVolumeInformationU : function (lpRootPathName: LPWSTR;
    lpVolumeNameBuffer: LPWSTR; nVolumeNameSize: DWORD;
    lpVolumeSerialNumber: PDWORD; lpMaximumComponentLength: PDWORD;
    lpFileSystemFlags: PDWORD;
    lpFileSystemNameBuffer: LPWSTR; nFileSystemNameSize: DWORD): BOOL; stdcall = nil;

function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
end;

Function FindMatch(var FindData: TWin32FindDataW; var f: TSearchRecUInt) : Longint;
begin
  { Find file with correct attribute }
  While (FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not FindNextFileU (F.FindHandle, FindData) then
      begin
        Result:=GetLastError;
        exit;
      end;
   end;
  { Convert some attributes back }
  WinToDosTime(FindData.ftLastWriteTime,F.Time);
  f.size:=FindData.NFileSizeLow+(qword(maxdword)+1)*FindData.NFileSizeHigh;
  f.attr:=FindData.dwFileAttributes;
  f.Name:=FindData.cFileName;
  Result:=0;
end;

function FindFirstWin(const Path: UnicodeString; Attr: Longint; out Rslt: TSearchRecUInt): Longint;
var
  find  : TWIN32FINDDATAW;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);

  Rslt.FindHandle:=FindFirstFileU( PWideChar(Path),find);
  If Rslt.FindHandle=Invalid_Handle_value then
  begin
    Result:=GetLastError;
    Exit;
  end;
  { Find file with correct attribute }
  Result:=FindMatch(find, Rslt);
end;

function FindNextWin(var Rslt: TSearchRecUInt): Longint;
var
  find  : TWIN32FINDDATAW;
begin
  if FindNextFileU(Rslt.FindHandle, find) then
    Result := FindMatch(find, Rslt)
  else
    Result := GetLastError;
end;

Procedure FindCloseWin(Var F : TSearchRecUInt);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
    Windows.FindClose(F.FindHandle);
end;

function FileGetAttrWin(const FileName : UnicodeString) : DWord;
begin
  Result:=GetFileAttributesU(PWideChar(FileName));
end;

function FileSetAttrWin(const Filename : UnicodeString; Attr: DWord) : Longint;
begin
  if SetFileAttributesU(PWideChar(FileName), Attr) then
    Result:=0
  else
    Result := GetLastError;
end;

function FileOpenWin(const FileName: UnicodeString; Mode : Integer): THandle;
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
  Result := CreateFileU(PWideChar(FileName), dword(AccessMode[Mode and 3]),
                       dword(ShareMode[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;

function FileCreateWin(const FileName: UnicodeString; Mode : Integer): THandle;
begin
  Result := CreateFileU(PWidechar(FileName), GENERIC_READ or GENERIC_WRITE,
              0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

function CreateDirWin(const Dir: UnicodeString): Boolean;
begin
  Result:=CreateDirectoryU(PUnicodeChar(Dir), nil);
end;

function RemoveDirWin(const Dir: UnicodeString): Boolean;
begin
  Result:=RemoveDirectoryU(PUnicodeChar(Dir));
end;

function SetCurrentDirWin(const Dir: UnicodeString): Boolean;
begin
  Result:=SetCurrentDirectoryU (PWideChar(Dir));
end;

function GetCurrentDirWin: UnicodeString;
var
  len : Integer;
begin
  len:=GetCurrentDirectoryU(0, nil);
  if len>0 then begin
    SetLength(Result, len);
    GetCurrentDirectoryU(length(Result), @Result[1]);
    Result:=Copy(Result, 1, len-1);
  end else
    Result:='';
end;

function DeleteFileWin(const FileName: UnicodeString): Boolean;
begin
  Result:=DeleteFileU(PUnicodeChar(FileName));
end;

function RenameFileWin(const OldName, NewName: UnicodeString): Boolean;
begin
  Result:=MoveFileU(PUnicodeChar(OldName), PUnicodeChar(NewName));
end;


function isUNCPath(const Path: UnicodeString): Boolean;
begin
  Result:=(Path<>'') and (Path[1] = '\') and (Path[2] = '\');
end;

function ExtractUNCRoot(const Path: UnicodeString): UnicodeString;
var
  i      : Integer;
  j      : Integer;
  slash1 : Integer;
begin
  slash1:=0;
  j:=length(Path)+1;
  for i:=3 to length(Path) do
    if (Path[i]='\') then begin
      if slash1 = 0 then
        slash1 := i
      else begin
        j:=i;
        Break;
      end;
    end;
  if slash1 > 0 then Result:=Copy(Path, 1, j)
  else Result:='';
end;

function ExtractDriveOrUNCRoot(const Path: UnicodeString): UnicodeString;
begin
  if isUNCPath(Path) then
    Result:=ExtractUNCRoot(Path)
  else
    Result:=ExtractFileDrive(Path);
end;

const
  FILE_CASE_SENSITIVE_SEARCH = $00000001;

function PathCaseSensitiveWin(const Path: UnicodeString; var CaseSensitive: Boolean): Boolean;
var
  Dir   : UnicodeString;
  Name  : UnicodeString;
  Flags : DWORD;
begin
  Dir:=IncludeTrailingPathDelimiter(ExtractDriveOrUNCRoot(Path));
  SetLength(Name, MAX_PATH+1);
  Result:=GetVolumeInformationU(PUnicodeChar(Dir), nil, 0, nil, nil, @Flags, nil, 0);
  if Result then CaseSensitive:=(Flags and FILE_CASE_SENSITIVE_SEARCH) > 0;
end;

procedure InitWindows;
begin
  kernel32dll:=LoadLibrary('kernel32.dll');
  if kernel32dll=0 then Exit;

  Pointer(FindFirstFileU) := GetProcAddress(kernel32dll, 'FindFirstFileW');
  Pointer(FindNextFileU) := GetProcAddress(kernel32dll, 'FindNextFileW');

  Pointer(SetFileAttributesU) := GetProcAddress(kernel32dll, 'SetFileAttributesW');
  Pointer(GetFileAttributesU) := GetProcAddress(kernel32dll, 'GetFileAttributesW');

  Pointer(CreateFileU) := GetProcAddress(kernel32dll, 'CreateFileW');

  Pointer(CreateDirectoryU) := GetProcAddress(kernel32dll, 'CreateDirectoryW');
  Pointer(RemoveDirectoryU) := GetProcAddress(kernel32dll, 'RemoveDirectoryW');
  Pointer(SetCurrentDirectoryU) := GetProcAddress(kernel32dll, 'SetCurrentDirectoryW');
  Pointer(GetCurrentDirectoryU) := GetProcAddress(kernel32dll, 'GetCurrentDirectoryW');

  Pointer(DeleteFileU) := GetProcAddress(kernel32dll, 'DeleteFileW');
  Pointer(MoveFileU) := GetProcAddress(kernel32dll, 'MoveFileW');
  Pointer(GetVolumeInformationU) := GetProcAddress(kernel32dll, 'GetVolumeInformationW');

  if Assigned(FindFirstFileU) and Assigned(FindNextFileU) then begin
    DoFindFirst:=@FindFirstWin;
    DoFindNext:=@FindNextWin;
    DoFindClose:=@FindCloseWin;
  end;

  if Assigned(SetFileAttributesU) and Assigned(GetFileAttributesU) then begin
    DoFileGetAttr:=@FileGetAttrWin;
    DoFileSetAttr:=@FileSetAttrWin;
  end;

  if Assigned(CreateFileU) then begin
    DoFileOpen:=@FileOpenWin;
    DoFileCreate:=@FileCreateWin;
  end;

  if Assigned(CreateDirectoryU) then begin
    DoCreateDir:=@CreateDirWin;
    DoRemoveDir:=@RemoveDirWin;
  end;

  if Assigned(CreateDirectoryU) then begin
    DoSetCurrentDir:=@SetCurrentDirWin;
    DoGetCurrentDir:=@GetCurrentDirWin;
  end;

  if Assigned(DeleteFileU) then DoDeleteFile:=@DeleteFileWin;
  if Assigned(MoveFileU) then DoRenameFile:=@RenameFileWin;
  if Assigned(GetVolumeInformationU) then DoPathCaseSensitive:=@PathCaseSensitiveWin;
end;

procedure ReleaseWindows;
begin
  if kernel32dll<>0 then FreeLibrary(kernel32dll);
end;

initialization
  InitWindows;

finalization
  ReleaseWindows;

end.
