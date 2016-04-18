unit ufilesutf8;

interface

{$mode objfpc}{$h+}

uses
  ufilescore;

// the function registers
//   DoFNUnicodeToAnsi
//   DoFNAnsiToUnicode
// to treat ansi file names as utf8 encoded.
// if RTL "string" type is changed, these functions might not be required
procedure RegisterUtf8AsAnsiNames;

implementation

function Utf8FNUnicodeToStr(const s: UnicodeString): string;
begin
  Result:=UTF8Encode(s);
end;

function Utf8FNStrToUnicode(const s: string): UnicodeString;
begin
  Result:=UTF8Decode(s);
end;

procedure RegisterUtf8AsAnsiNames;
begin
  DoFNUnicodeToStr:=@Utf8FNUnicodeToStr;
  DoFNStrToUnicode:=@Utf8FNStrToUnicode;
end;


end.
