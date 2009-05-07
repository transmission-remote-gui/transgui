{
    This file is part of the Free Component Library

    JSON source parser
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit jsonparser;

interface

uses
  Classes, SysUtils, fpJSON, jsonscanner;
  
Type

  { TJSONParser }

  TJSONParser = Class(TObject)
  Private
    FScanner : TJSONScanner;
    function ParseNumber: TJSONNumber;
  Protected
    procedure DoError(Msg: String);
    function DoParse(AtCurrent,AllowEOF: Boolean): TJSONData;
    function GetNextToken: TJSONToken;
    function CurrentTokenString: TJSONStringType;
    function CurrentToken: TJSONToken;
    function ParseArray: TJSONArray;
    function ParseObject: TJSONObject;
  Public
    function Parse: TJSONData;
    Constructor Create(Source : TStream); overload;
    Constructor Create(Source : TJSONStringType); overload;
    destructor Destroy();override;
  end;
  
  EJSONScanner = Class(Exception);
  
implementation

Resourcestring
  SErrUnexpectedEOF   = 'Unexpected EOF encountered.';
  SErrUnexpectedToken = 'Unexpected token (%s) encountered.';
  SErrExpectedColon   = 'Expected colon (:), got token "%s".';
  SErrUnexpectedComma = 'Invalid comma encountered.';
  SErrEmptyElement = 'Empty element encountered.';
  SErrExpectedElementName    = 'Expected element name, got token "%s"';
  SExpectedCommaorBraceClose = 'Expected , or ], got token "%s".';
  SErrInvalidNumber          = 'Number is not an integer or real number: %s';
  
{ TJSONParser }

Function TJSONParser.Parse : TJSONData;

begin
  Result:=DoParse(False,True);
end;

{
  Consume next token and convert to JSON data structure.
  If AtCurrent is true, the current token is used. If false,
  a token is gotten from the scanner.
  If AllowEOF is false, encountering a tkEOF will result in an exception.
}

Function TJSONParser.CurrentToken : TJSONToken;

begin
  Result:=FScanner.CurToken;
end;

Function TJSONParser.CurrentTokenString : TJSONStringType;

begin
  If CurrentToken in [tkString,tkNumber] then
    Result:=FScanner.CurTokenString
  else
    Result:=TokenInfos[CurrentToken];
end;

Function TJSONParser.DoParse(AtCurrent,AllowEOF : Boolean) : TJSONData;

var
  T : TJSONToken;
  
begin
  Result:=nil;
  try
    If not AtCurrent then
      T:=GetNextToken
    else
      T:=FScanner.CurToken;
    Case T of
      tkEof : If Not AllowEof then
                DoError(SErrUnexpectedEOF);
      tkNull  : Result:=TJSONNull.Create;
      tkTrue,
      tkFalse : Result:=TJSONBoolean.Create(t=tkTrue);
      tkString : Result:=TJSONString.Create(CurrentTokenString);
      tkCurlyBraceOpen : Result:=ParseObject;
      tkCurlyBraceClose : DoError(SErrUnexpectedToken);
      tkSQuaredBraceOpen : Result:=ParseArray;
      tkSQuaredBraceClose : DoError(SErrUnexpectedToken);
      tkNumber : Result:=ParseNumber;
      tkComma : DoError(SErrUnexpectedToken);
    end;
  except
    if assigned(Result) then
      FreeAndNil(Result);
    Raise;
  end;
end;


// Creates the correct JSON number type, based on the current token.
Function TJSONParser.ParseNumber : TJSONNumber;

Var
  I : Int64;
  F : TJSONFloat;
  S : TJSONStringType;

begin
  S:=CurrentTokenString;
  I:=0;
  If TryStrToInt64(S,I) and (I >= Low(LongInt)) and (I <= High(LongInt)) then
    Result:=TJSONIntegerNumber.Create(I)
  else
    begin
    I:=0;
    Val(S,F,I);
    If (I<>0) then
      DoError(SErrInvalidNumber);
    Result:=TJSONFloatNumber.Create(F);
    end;
end;

// Current token is {, on exit current token is }
Function TJSONParser.ParseObject : TJSONObject;

Var
  T : TJSONtoken;
  E : TJSONData;
  N : TJSONStringType;
  
begin
  Result:=TJSONObject.Create;
  Try
    T:=GetNextToken;
    While T<>tkCurlyBraceClose do
      begin
      If T<>tkString then
        DoError(SErrExpectedElementName);
      N:=CurrentTokenString;
      T:=GetNextToken;
      If (T<>tkColon) then
        DoError(SErrExpectedColon);
      E:=DoParse(False,False);
      Result.Add(N,E);
      T:=GetNextToken;
      If Not (T in [tkComma,tkCurlyBraceClose]) then
        DoError(SExpectedCommaorBraceClose);
      If T=tkComma then
        T:=GetNextToken;
      end;
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;

// Current token is [, on exit current token is ]
Function TJSONParser.ParseArray : TJSONArray;

Var
  T : TJSONtoken;
  E : TJSONData;
  LastComma : Boolean;
  
begin
  Result:=TJSONArray.Create;
  LastComma:=False;
  Try
    Repeat
      T:=GetNextToken;
      If (T<>tkSquaredBraceClose) then
        begin
        E:=DoParse(True,False);
        If (E<>Nil) then
          Result.Add(E)
        else if (Result.Count>0) then
          DoError(SErrEmptyElement);
        T:=GetNextToken;
        If Not (T in [tkComma,tkSquaredBraceClose]) then
          DoError(SExpectedCommaorBraceClose);
        LastComma:=(t=TkComma);
        end;
    Until (T=tkSquaredBraceClose);
    If LastComma then // Test for ,] case
      DoError(SErrUnExpectedToken);
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;

// Get next token, discarding whitespace
Function TJSONParser.GetNextToken : TJSONToken ;

begin
  Repeat
    Result:=FScanner.FetchToken;
  Until (Result<>tkWhiteSpace);
end;

Procedure TJSONParser.DoError(Msg : String);

Var
  S : TJSONStringType;

begin
  S:=Format(Msg,[CurrentTokenString]);
  S:=Format('Error at line %d, Pos %d:',[FScanner.CurRow,FSCanner.CurColumn])+S;
  Raise EJSONScanner.Create(S);
end;

constructor TJSONParser.Create(Source: TStream);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source);
end;

constructor TJSONParser.Create(Source: TJSONStringType);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source);
end;

destructor TJSONParser.Destroy();
begin
  FreeAndNil(FScanner);
  inherited Destroy();
end;

end.

