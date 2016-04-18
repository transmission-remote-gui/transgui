unit
  ufilescore;

interface

{$mode objfpc}{$h+}

uses
  SysUtils;

Type
  TSearchRecUInt = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : UnicodeString;
    ExcludeAttr : LongInt;
    {$ifdef unix}
    FindHandle : Pointer;
    //mode?
    {$else}
    FindHandle : THandle;
    {$endif}
  end;

function CreateDirAnsi (const Dir: UnicodeString): Boolean;
function RemoveDirAnsi (const Dir: UnicodeString): Boolean;
function SetCurrentDirAnsi (const Dir: UnicodeString): Boolean;
function GetCurrentDirAnsi: UnicodeString;
function DeleteFileAnsi(const FileName : UnicodeString): Boolean;
function RenameFileAnsi(const OldName, NewName : UnicodeString): Boolean;
function FileCreateAnsi(const FileName : UnicodeString; Mode : Integer) : THandle;
function FileOpenAnsi(const FileName : UnicodeString; Mode : Integer): THandle;
procedure FindCloseAnsi (var F : TSearchrecUInt);
function FindNextAnsi (var Rslt : TSearchrecUInt) : Longint;
function FindFirstAnsi (Const Path : UnicodeString; Attr : Longint; out Rslt : TSearchrecUInt) : Longint;
function FileGetAttrAnsi (const FileName : UnicodeString) : DWORD;
function FileSetAttrAnsi (const Filename : UnicodeString; Attr: DWORD) : Longint;

procedure SearchRecAnsiToUnicode(const src: TSearchRec; var dst: TSearchRecUint);
procedure SearchRecUnicodeToAnsi(const src: TSearchRecUInt; var dst: TSearchRec);

function DefPathCaseSensetive(const Path: UnicodeString; var isCaseSensitive: Boolean): Boolean;
function DefCompareFileName(const S1, S2: UnicodeString; CaseSensitive: Boolean): Integer;

function DefFNUnicodeToStr(const s: UnicodeString): String;
function DefFNStrToUnicode(const s: String): UnicodeString;

type
  TAnsiCharSet = set of AnsiChar;


// Cross-platform part
// Do* functions are system dependable and can be reassigned, if necessary

var
  DoFindFirst : function  (const Path : UnicodeString; Attr : Longint; out Rslt : TSearchrecUInt) : Longint = @FindFirstAnsi;
  DoFindNext  : function  (var Rslt : TSearchRecUInt) : Longint = @FindNextAnsi;
  DoFindClose : procedure (var F : TSearchrecUInt) = @FindCloseAnsi;
  DoFileGetAttr : function (const FileName : UnicodeString) : DWORD = @FileGetAttrAnsi;
  DoFileSetAttr : function (const Filename : UnicodeString; Attr: DWORD) : Longint = @FileSetAttrAnsi;
  DoFileOpen    : function (const FileName : UnicodeString; Mode : Integer) : THandle = @FileOpenAnsi;
  DoFileCreate  : function (const FileName : UnicodeString; Mode : Integer) : THandle = @FileCreateAnsi;

  DoCreateDir : function (const Dir: UnicodeString): Boolean = @CreateDirAnsi;
  DoRemoveDir : function (const Dir: UnicodeString): Boolean = @RemoveDirAnsi;
  DoSetCurrentDir : function (const Dir: UnicodeString): Boolean = @SetCurrentDirAnsi;
  DoGetCurrentDir : function : UnicodeString = @GetCurrentDirAnsi;

  DoDeleteFile : function (const FileName : UnicodeString): Boolean = @DeleteFileAnsi;
  DoRenameFile : function (const OldName, NewName : UnicodeString): Boolean = @RenameFileAnsi;

  DoPathCaseSensitive: function (const FilePath: UnicodeString; var isCaseSensitive: Boolean): Boolean = @DefPathCaseSensetive;
  DoCompareFileName : function (const S1, S2: UnicodeString; CaseSensitive: Boolean): Integer = @DefCompareFileName;

  // these functions are used by *ANSI functions to convert between unicode and ansi names
  DoFNUnicodeToStr : function (const s: UnicodeString): String = @DefFNUnicodeToStr;
  DoFNStrToUnicode : function (const s: String): UnicodeString = @DefFNStrToUnicode;


implementation

function FindFirstAnsi (Const Path : UnicodeString; Attr : Longint; out Rslt : TSearchRecUInt) : Longint;
var
  sr : TSearchRec;
begin
  Result:=SysUtils.FindFirst(  DoFNUnicodeToStr(Path), Attr, sr);
  if Result=0 then SearchRecAnsiToUnicode(sr, Rslt)
end;

function FindNextAnsi (var Rslt : TSearchRecUInt) : Longint;
var
  sr : TSearchRec;
begin
  SearchRecUnicodeToAnsi(Rslt, sr);
  Result:=SysUtils.FindNext(sr);
  SearchRecAnsiToUnicode(sr, Rslt);
end;

procedure FindCloseAnsi (var F : TSearchRecUInt);
var
  sr : TSearchRec;
begin
  SearchRecUnicodeToAnsi(F, sr);
  SysUtils.FindClose(sr);
end;

function FileOpenAnsi(const FileName : UnicodeString; Mode : Integer): THandle;
begin
  Result:=SysUtils.FileOpen( DoFNUnicodeToStr(FileName), Mode);
end;

function FileCreateAnsi(const FileName : UnicodeString; Mode : Integer) : THandle;
begin
  Result:=SysUtils.FileCreate( DoFNUnicodeToStr(FileName), Mode);
end;

function CreateDirAnsi (const Dir: UnicodeString): Boolean;
begin
  Result:=SysUtils.CreateDir(DoFNUnicodeToStr(Dir));
end;

function RemoveDirAnsi (const Dir: UnicodeString): Boolean;
begin
  Result:=SysUtils.RemoveDir(DoFNUnicodeToStr(Dir));
end;

function SetCurrentDirAnsi (const Dir: UnicodeString): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(DoFNUnicodeToStr(Dir));
end;

function GetCurrentDirAnsi: UnicodeString;
begin
  Result:=DoFNStrToUnicode(SysUtils.GetCurrentDir);
end;

function DeleteFileAnsi(const FileName : UnicodeString): Boolean;
begin
  Result:=SysUtils.DeleteFile( DoFNUnicodeToStr(FileName) );
end;

function RenameFileAnsi(const OldName, NewName : UnicodeString): Boolean;
begin
  Result:=SysUtils.RenameFile( DoFNUnicodeToStr(OldName), DoFNUnicodeToStr(NewName));
end;

function FileGetAttrAnsi (const FileName : UnicodeString) : DWord;
begin
  Result:=SysUtils.FileGetAttr( DoFNUnicodeToStr(FileName) );
end;

function FileSetAttrAnsi (const Filename : UnicodeString; Attr: DWord) : Longint;
begin
  Result:=SysUtils.FileSetAttr( DoFNUnicodeToStr(FileName), Attr);
end;

procedure SearchRecAnsiToUnicode(const src: TSearchRec; var dst: TSearchRecUInt);
begin
  with dst do begin
    Time := src.Time;
    Size := src.Size;
    Attr := src.Attr;
    Name := DoFNStrToUnicode(src.Name);
    FindHandle := src.FindHandle;
  end;
end;

procedure SearchRecUnicodeToAnsi(const src: TSearchRecUInt; var dst: TSearchRec);
begin
  FillChar(dst, sizeof(dst), 0);
  with dst do begin
    Time := src.Time;
    Size := src.Size;
    Attr := src.Attr;
    Name := DoFNUnicodeToStr(src.Name);
    FindHandle := src.FindHandle;
  end;
end;

function DefCompareFileName(const S1, S2: UnicodeString; CaseSensitive: Boolean): Integer;
begin
  if not CaseSensitive then
    Result:=UnicodeCompareText(UnicodeLowerCase(S1), UnicodeLowerCase(S2))
  else
    Result:=UnicodeCompareText(S1, S2);
end;

function DefPathCaseSensetive(const Path: UnicodeString; var isCaseSensitive: Boolean): Boolean;
begin
  isCaseSensitive:=System.FileNameCaseSensitive;
  Result:=True;
end;

function DefFNUnicodeToStr(const s: UnicodeString): String;
begin
  Result:=s; // using UnicodeString manager
end;

function DefFNStrToUnicode(const s: String): UnicodeString;
begin
  Result:=s; // using UnicodeString manager
end;


initialization

end.
