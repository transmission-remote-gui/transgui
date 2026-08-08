unit FileTemplates;

{$mode objfpc}{$H+}

interface

type
  TFileTemplates = array of string;

function ParseFileTemplates(const Value: string): TFileTemplates;

implementation

uses
  SysUtils;

function ParseFileTemplates(const Value: string): TFileTemplates;
var
  i, TokenCount, TokenStart, ValueLength: integer;
  Token: string;
begin
  ValueLength:=Length(Value);
  TokenCount:=0;
  i:=1;
  while i <= ValueLength do begin
    while (i <= ValueLength) and (Value[i] = ' ') do
      Inc(i);
    if i > ValueLength then
      break;
    Inc(TokenCount);
    while (i <= ValueLength) and (Value[i] <> ' ') do
      Inc(i);
  end;

  SetLength(Result, TokenCount);
  TokenCount:=0;
  i:=1;
  while i <= ValueLength do begin
    while (i <= ValueLength) and (Value[i] = ' ') do
      Inc(i);
    if i > ValueLength then
      break;
    TokenStart:=i;
    while (i <= ValueLength) and (Value[i] <> ' ') do
      Inc(i);
    Token:=Trim(Copy(Value, TokenStart, i - TokenStart));
    if Token <> '' then begin
      Result[TokenCount]:=Token;
      Inc(TokenCount);
    end;
  end;
  SetLength(Result, TokenCount);
end;

end.
