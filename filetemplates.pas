unit FileTemplates;

{$mode objfpc}{$H+}

interface

type
  TFileTemplates = array of string;

function ParseFileTemplates(const Value: string): TFileTemplates;
function IsFileTemplate(const FileName: string;
  const Templates: array of string): boolean;

implementation

uses
  SysUtils;

function ParseFileTemplates(const Value: string): TFileTemplates;
var
  i, TokenCount, TokenStart, ValueLength: integer;
  Token: string;
begin
  Result:=nil;
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

function IsFileTemplate(const FileName: string;
  const Templates: array of string): boolean;
var
  tmp, tmpExt, tmp_Name, sstr: string;
  i,n,lstr,j,k, total_sstr, total_templ : integer;
  ok : boolean;
  re : boolean;
begin
  Result:=false;

  tmp:=FileName;
  lstr:=Length(tmp);
  for i:=1 to lstr-1 do begin
    if (tmp[lstr-i]= '/') or (tmp[lstr-i]= '\') then begin
      tmp:=Copy(tmp, lstr-i+1, Length(tmp));
      Break;
    end;
  end;

  for i:=0 to High(Templates) do begin
    tmpExt:=Templates[i];
    tmp_Name:=tmp;
    total_sstr:=0;
    total_templ:=0;
    while tmpExt <> '' do begin
      j:=Pos('*', tmpExt);
      if j <> 0 then begin
        sstr:=Copy(tmpExt, 1, j-1);
        tmpExt:=Copy(tmpExt, j+1, Length(tmpExt));
        re:=false;
      end
      else begin
        sstr:=Trim(tmpExt);
        tmpExt:='';
        re:=true;
      end;
      if sstr = '' then
        continue;

      Inc(total_templ);
      n:=Length(sstr);
      ok:=false;
      while true do begin
        if tmp_Name = '' then
          break;
        k:=Pos(sstr, tmp_Name);
        if k <> 0 then begin
          tmp_Name:=Copy(tmp_Name, k+n, Length(tmp_Name));
          if (tmpExt = '') and re and (tmp_Name <> '') then
            continue
          else begin
            Inc(total_sstr);
            ok:=true;
            Break;
          end;
        end
        else
          Break;
      end;
      if ok then
        Break;
    end;

    if total_sstr = total_templ then begin
      Result:=true;
      Break;
    end;
  end;
end;

end.
