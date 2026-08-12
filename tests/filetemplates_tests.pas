unit FileTemplates_Tests;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FileTemplates;

type
  TFileTemplatesTest = class(TTestCase)
  private
    procedure CheckTemplates(const Input: string; const Expected: array of string);
    function BuildTemplates(Count: integer): string;
  published
    procedure ParsesEmptyInput;
    procedure ParsesSingleTemplate;
    procedure ParsesMultipleTemplates;
    procedure IgnoresRepeatedSpaces;
    procedure PreservesTemplateText;
    procedure SplitsOnlyOnSpaces;
    procedure TrimsControlWhitespaceAtTemplateBoundaries;
    procedure ParsesBeyondLegacyCapacity;
    procedure ParsesInputLongerThanLegacyCopyLimit;
    procedure MatchesLongPathBasename;
    procedure MatchesLongTemplateSuffix;
    procedure FindsFinalMatchBeyondLegacyCopyLimit;
  end;

implementation

procedure TFileTemplatesTest.CheckTemplates(const Input: string;
  const Expected: array of string);
var
  Actual: TFileTemplates;
  i: integer;
begin
  Actual:=ParseFileTemplates(Input);
  AssertEquals('template count', Length(Expected), Length(Actual));
  for i:=Low(Expected) to High(Expected) do
    AssertEquals('template ' + IntToStr(i), Expected[i], Actual[i]);
end;

function TFileTemplatesTest.BuildTemplates(Count: integer): string;
var
  i: integer;
begin
  Result:='';
  for i:=1 to Count do begin
    if Result <> '' then
      Result:=Result + ' ';
    Result:=Result + 'template' + IntToStr(i);
  end;
end;

procedure TFileTemplatesTest.ParsesEmptyInput;
begin
  CheckTemplates('', []);
end;

procedure TFileTemplatesTest.ParsesSingleTemplate;
begin
  CheckTemplates('*.avi', ['*.avi']);
end;

procedure TFileTemplatesTest.ParsesMultipleTemplates;
begin
  CheckTemplates('*.avi *.mkv tom*jerry*.avi',
    ['*.avi', '*.mkv', 'tom*jerry*.avi']);
end;

procedure TFileTemplatesTest.IgnoresRepeatedSpaces;
begin
  CheckTemplates('  *.avi   *.mkv  ', ['*.avi', '*.mkv']);
end;

procedure TFileTemplatesTest.PreservesTemplateText;
begin
  CheckTemplates('*.AVI Mixed*Case.MKV', ['*.AVI', 'Mixed*Case.MKV']);
end;

procedure TFileTemplatesTest.SplitsOnlyOnSpaces;
begin
  CheckTemplates('*.avi' + #9 + '*.mkv *.mp4',
    ['*.avi' + #9 + '*.mkv', '*.mp4']);
end;

procedure TFileTemplatesTest.TrimsControlWhitespaceAtTemplateBoundaries;
begin
  CheckTemplates(#9 + '*.avi' + #9 + ' ' + #10 + '*.mkv' + #13 +
    ' ' + #9 + ' *.mp4' + #9, ['*.avi', '*.mkv', '*.mp4']);
end;

procedure TFileTemplatesTest.ParsesBeyondLegacyCapacity;
var
  Actual: TFileTemplates;
  Input: string;
begin
  Input:=BuildTemplates(52);
  Actual:=ParseFileTemplates(Input);
  AssertEquals('template count', 52, Length(Actual));
  AssertEquals('first template', 'template1', Actual[0]);
  AssertEquals('last template', 'template52', Actual[51]);
end;

procedure TFileTemplatesTest.ParsesInputLongerThanLegacyCopyLimit;
var
  Actual: TFileTemplates;
  Input: string;
begin
  Input:=BuildTemplates(200);
  AssertTrue('test input must exceed 999 characters', Length(Input) > 999);
  Actual:=ParseFileTemplates(Input);
  AssertEquals('template count', 200, Length(Actual));
  AssertEquals('last template', 'template200', Actual[199]);
end;

procedure TFileTemplatesTest.MatchesLongPathBasename;
var
  BaseName: string;
begin
  BaseName:=StringOfChar('a', 1000) + '.mkv';
  AssertTrue('long basename after path separator must remain intact',
    IsFileTemplate('folder/' + BaseName, [BaseName]));
end;

procedure TFileTemplatesTest.MatchesLongTemplateSuffix;
var
  Suffix: string;
begin
  Suffix:=StringOfChar('b', 1000) + '.avi';
  AssertTrue('template remainder after wildcard must remain intact',
    IsFileTemplate(Suffix, ['*' + Suffix]));
end;

procedure TFileTemplatesTest.FindsFinalMatchBeyondLegacyCopyLimit;
var
  FileName: string;
begin
  FileName:='end' + StringOfChar('x', 1000) + 'end';
  AssertTrue('filename remainder must not be truncated before the final match',
    IsFileTemplate(FileName, ['end']));
end;

initialization
  RegisterTest(TFileTemplatesTest);

end.
