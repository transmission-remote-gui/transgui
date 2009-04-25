{
    This file is part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit jsonscanner;

interface

uses SysUtils, Classes;

resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'string exceeds end of line';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkUnknown
    );

  EScannerError       = class(Exception);


  TJSONScanner = class
  private
    FSource : TStringList;
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenString: widestring;
    FCurLine: string;
    TokenStr: PChar;
    function GetCurColumn: Integer;
  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string; Args: array of Const);overload;
    function DoFetchToken: TJSONToken;
  public
    constructor Create(Source : TStream); overload;
    constructor Create(Source : String); overload;
    destructor Destroy; override;
    function FetchToken: TJSONToken;


    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenString: widestring read FCurTokenString;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    ''
  );


implementation

constructor TJSONScanner.Create(Source : TStream);

begin
  FSource:=TStringList.Create;
  FSource.LoadFromStream(Source);
end;

constructor TJSONScanner.Create(Source : String);
begin
  FSource:=TStringList.Create;
  FSource.Text:=Source;
end;

destructor TJSONScanner.Destroy;
begin
  FreeAndNil(FSource);
  Inherited;
end;


function TJSONScanner.FetchToken: TJSONToken;
  
begin
  Result:=DoFetchToken;
end;

procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string; Args: array of Const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.DoFetchToken: TJSONToken;

  function FetchLine: Boolean;
  begin
    Result:=FCurRow<FSource.Count;
    if Result then
      begin
      FCurLine:=FSource[FCurRow];
      TokenStr:=PChar(FCurLine);
      Inc(FCurRow);
      end
    else             
      begin
      FCurLine:='';
      TokenStr:=nil;
      end;
  end;

var
  TokenStart: PChar;
  it : TJSONToken;
  I : Integer;
  SectionLength: Integer;
  S : WideString;
  SS : AnsiString;

begin
  if TokenStr = nil then
    if not FetchLine then
      begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
      end;

  FCurTokenString := '';

  case TokenStr[0] of
    #0:         // Empty line
      begin
      FetchLine;
      Result := tkWhitespace;
      end;
    #9, ' ':
      begin
      Result := tkWhitespace;
      repeat
        Inc(TokenStr);
        if TokenStr[0] = #0 then
          if not FetchLine then
          begin
            FCurToken := Result;
            exit;
          end;
      until not (TokenStr[0] in [#9, ' ']);
      end;
    '"':
      begin
        Inc(TokenStr);
        TokenStart := TokenStr;
        FCurTokenString := '';

        while not (TokenStr[0] in [#0,'"']) do
          begin
          if (TokenStr[0]='\') then
            begin
            // Save length
            SectionLength := TokenStr - TokenStart;
            Inc(TokenStr);
            // Read escaped token
            Case TokenStr[0] of
              '"' : S:='"';
              't' : S:=#9;
              'b' : S:=#8;
              'n' : S:=#10;
              'r' : S:=#13;
              'f' : S:=#12;
              '\' : S:='\';
              '/' : S:='/';
              'u' : begin
                    S:='0000';
                    For I:=1 to 4 do
                      begin
                      Inc(TokenStr);
                      Case TokenStr[0] of
                        '0'..'9','A'..'F','a'..'f' :
                          S[i]:=Upcase(TokenStr[0]);
                      else
                        Error(SErrInvalidCharacter, [TokenStr[0]]);
                      end;
                      end;
                    // Takes care of conversion...  
                    S:=WideChar(StrToInt('$'+S));
                    end;
              #0  : Error(SErrOpenString);
            else
              Error(SErrInvalidCharacter, [TokenStr[0]]);  
            end;
            SetString(SS, TokenStart, SectionLength);
            FCurTokenString:=FCurTokenString + widestring(SS) + S;
            // Next char
            // Inc(TokenStr);
            TokenStart := TokenStr+1;
            end;
          if TokenStr[0] = #0 then
            Error(SErrOpenString);
          Inc(TokenStr);
          end;
        if TokenStr[0] = #0 then
          Error(SErrOpenString);
        SectionLength := TokenStr - TokenStart;
        SetString(SS, TokenStart, SectionLength);
        FCurTokenString:=FCurTokenString + SS;
        Inc(TokenStr);
        Result := tkString;
      end;
    ',':
      begin
        Inc(TokenStr);
        Result := tkComma;
      end;
    '0'..'9','-':
      begin
        TokenStart := TokenStr;
        while true do
        begin
          Inc(TokenStr);
          case TokenStr[0] of
            '.':
              begin
                if TokenStr[1] in ['0'..'9', 'e', 'E'] then
                begin
                  Inc(TokenStr);
                  repeat
                    Inc(TokenStr);
                  until not (TokenStr[0] in ['0'..'9', 'e', 'E','-','+']);
                end;
                break;
              end;
            '0'..'9': ;
            'e', 'E':
              begin
                Inc(TokenStr);
                if TokenStr[0] in ['-','+']  then
                  Inc(TokenStr);
                while TokenStr[0] in ['0'..'9'] do
                  Inc(TokenStr);
                break;
              end;
            else
              break;
          end;
        end;
        SectionLength := TokenStr - TokenStart;
        SetString(SS, TokenStart, SectionLength);
        FCurTokenString:=SS;
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(TokenStr);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(TokenStr);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(TokenStr);
        Result := tkCurlyBraceClose;
      end;  
    '[':
      begin
        Inc(TokenStr);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(TokenStr);
        Result := tkSquaredBraceClose;
      end;
    'T','t','F','f','N','n' :
      begin
        TokenStart := TokenStr;
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        SectionLength := TokenStr - TokenStart;
        SetString(SS, TokenStart, SectionLength);
        FCurTokenString:=SS;
        for it := tkTrue to tkNull do
          if CompareText(CurTokenString, TokenInfos[it]) = 0 then
            begin
            Result := it;
            FCurToken := Result;
            exit;
            end;
        Error(SErrInvalidCharacter, [TokenStart[0]]);
      end;
  else
    Error(SErrInvalidCharacter, [TokenStr[0]]);
  end;

  FCurToken := Result;
end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PChar(CurLine);
end;

end.
