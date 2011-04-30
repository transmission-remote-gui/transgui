{************************************************************
   Copyright (c) 2010 Alex Cherednichenko, aka Alex7Che.

   Published at GNU General Public License as Free Software.
 ************************************************************}

unit ResTranslator;

{$MODE objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, FileUtil, LResources, TypInfo, LCLProc;

type

  TWordDelimitersOptions = set of (wdIgnoreLeading, wdIgnoreTrailing);

  TResTranslator = class;

  TTranslateStringEvent = procedure(Sender: TResTranslator; const ResourceName: AnsiString; var Accept: boolean);

  { TTranslateStringList }

  TTranslateStringList = class(TStringList)
  private
    function CorrectGetName(Index: integer): string;
    function CorrectGetValue(const Name: string): string;
    function NormaliseQuotedStr(const S: string): string;
    function ScanQuotSep(P: PChar):integer;
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  public
    constructor Create(const FileName: string); overload;
    function IndexOfName(const Name: string; var Offset: integer): integer;
    function IndexOfName(const Name: string): integer; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure Merge(Source: TTranslateStringList; const NamesOnly: boolean = false);
    property CValues[const Name: string]: string read CorrectGetValue;
    property CNames[Index: integer]: string read CorrectGetName;
  end;

  { TResTranslator }

  TResTranslator = class(TAbstractTranslator)
  private
    FIgnoreDelimiters: TWordDelimitersOptions;
    FOnTranslateString: TTranslateStringEvent;
    FStrResLst:    TTranslateStringList;
    FAddedStrings: TTranslateStringList;
    FStrings: TTranslateStringList;
    FTranslationFile: string;
    FModified:     boolean;
    FTranslationLanguage: AnsiString;
    FWordDelims:   TSysCharset;
    function GetAddedStrings: TStringList;
    function GetStrings: TStringList;
    procedure SetIgnoreDelimiters(const AValue: TWordDelimitersOptions);
    procedure SetOnTranslateString(const AValue: TTranslateStringEvent);
    procedure SetWordDelims(const AValue: TSysCharset);
    function InternalTranslateString(const Value: AnsiString): AnsiString;
  public
    constructor Create(TranslationFile: AnsiString);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content: string); override;
    procedure SaveFile; overload;
    procedure SaveFile(const aFileName: string); overload;
    property Modified: boolean Read FModified;
    property Strings: TStringList Read GetStrings;
    property AddedStrings: TStringList Read GetAddedStrings;
    property IgnoreDelimiters: TWordDelimitersOptions Read FIgnoreDelimiters Write SetIgnoreDelimiters;
    property WordDelims: TSysCharset Read FWordDelims Write SetWordDelims;
    property TranslationLanguage: AnsiString read FTranslationLanguage;
    property OnTranslateString: TTranslateStringEvent Read FOnTranslateString Write SetOnTranslateString;
  end;

function LoadTranslationFile(const TranslationFile: AnsiString; const OnTranslate: TTranslateStringEvent = nil): AnsiString;
procedure SaveTranslationFile; overload;
procedure SaveTranslationFile(const FileName: AnsiString); overload;
procedure MakeTranslationFile; overload;
procedure MakeTranslationFile(Language: AnsiString); overload;
procedure MakeTranslationFile(const FileName, Language: AnsiString); overload;
procedure SupplementTranslationFile(const FileName: AnsiString);
procedure SupplementTranslationFiles; overload;
procedure SupplementTranslationFiles(const TranslationFilesPath: AnsiString); overload;
function LoadDefaultTranslationFile(const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadDefaultTranslationFile(const TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadLanguageTranslation(const Language: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadLanguageTranslation(const Language, TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function TranslateString(const Value: AnsiString): AnsiString;
function ExtractLangName(const FileName: TFilename): AnsiString;
function GetAvailableTranslations: TStringList;
function GetAvailableTranslations(const SearchPath: AnsiString): TStringList;
function GetTranslationFileName(const Language: AnsiString; AvailableTranslations: TStringList): AnsiString;
function DefaultLangDir: AnsiString;

const
  sLanguageIDName = 'TranslationLanguage';

implementation

uses
  Forms;

const
  LineSeparator = '###################';

{ procedures and functions }

function IsQuoted(const S: AnsiString; QuoteChar: char): boolean; inline;
var
  L: integer;
begin
  L:= Length(S);
  if L > 1 then
    Result := (S[1] = QuoteChar) and (S[L] = QuoteChar)
  else
    Result := false;
end;

function HasSeparator(const S: AnsiString; Separator: char): boolean; inline;
begin
  Result := Pos(Separator, S) > 0;
end;

function ExtractLangName(const FileName: TFilename): AnsiString;
begin
  with TTranslateStringList.Create(FileName) do
    try
      Result := AnsiDequotedStr(CValues[sLanguageIDName], QuoteChar);
    finally
      Free;
    end;
end;

function GetAvailableTranslations: TStringList;
begin
  Result:= GetAvailableTranslations(DefaultLangDir);
end;

function GetAvailableTranslations(const SearchPath: AnsiString): TStringList;
var
  Sr: TSearchRec;
  LangName: AnsiString;
begin
  Result:= TStringList.Create;
  if FindFirst(IncludeTrailingPathDelimiter(SearchPath) + '*', faArchive or faReadOnly, Sr) = 0 then
    with Result do begin
      NameValueSeparator:= '=';
      QuoteChar:= '"';
      repeat
        if ExtractFileExt(Sr.Name) = '.template' then
          continue;
        LangName:= ExtractLangName(IncludeTrailingPathDelimiter(ExtractFilePath(SearchPath)) + Sr.Name);
        if LangName <> '' then
          Add(LangName + NameValueSeparator + Sr.Name);
      until FindNext(Sr) <> 0;
      FindClose(Sr);
    end;
end;

var
  FDefaultLangDir: AnsiString;

function DefaultLangDir: AnsiString;
{$ifdef unix}
  function _IsLangDir(const dir: string): boolean;
  var
    sr: TSearchRec;
  begin
    Result:=FindFirst(dir + ExtractFileNameOnly(ParamStr(0)) + '.*', faAnyFile, sr) = 0;
    FindClose(sr);
  end;

var
  s: string;
{$endif unix}
begin
  if FDefaultLangDir = '' then begin
    FDefaultLangDir:=ExtractFilePath(ParamStr(0)) + 'lang' + DirectorySeparator;
{$ifdef unix}
    if not _IsLangDir(FDefaultLangDir) then begin
      s:='/usr/share/' + ExtractFileNameOnly(ParamStr(0)) + '/lang/';
      if _IsLangDir(s) then
        FDefaultLangDir:=s
      else begin
        s:='/usr/local/share/' + ExtractFileNameOnly(ParamStr(0)) + '/lang/';
        if _IsLangDir(s) then
          FDefaultLangDir:=s;
      end;
    end;
{$endif unix}
  end;
  Result:=FDefaultLangDir;
end;

function GetResStrings(Name, Value: AnsiString; Hash: longint; P: pointer): AnsiString;
var
  Accept: boolean;
begin
  with TResTranslator(P) do begin
    Accept := True;
    if Assigned(OnTranslateString) then
      OnTranslateString(TResTranslator(P), Name, Accept);
    if Accept then
      Result := InternalTranslateString(Value)
    else
      Result := Value;
  end;
end;

function LoadTranslationFile(const TranslationFile: AnsiString; const OnTranslate: TTranslateStringEvent = nil): AnsiString;
begin
  LRSTranslator := TResTranslator.Create(TranslationFile);
  TResTranslator(LRSTranslator).OnTranslateString := OnTranslate;
  SetResourceStrings(@GetResStrings, LRSTranslator);
  Result := TResTranslator(LRSTranslator).TranslationLanguage;
end;

procedure SupplementTranslationFiles; overload;
begin
  SupplementTranslationFiles(DefaultLangDir);
end;

procedure MakeTranslationFile; overload;
begin
  MakeTranslationFile('???');
end;

procedure MakeTranslationFile(Language: AnsiString); overload;
var
  lLang, sLang, s: string;
begin
  LCLGetLanguageIDs(lLang, sLang);
  sLang:=AnsiLowerCase(sLang);
  s:=ExtractFileNameOnly(ParamStr(0));
  if (sLang <> '') and not FileExists(DefaultLangDir + s + '.' + sLang) then
    s:=s + '.' + sLang
  else
    s:=s + '.lng';
  MakeTranslationFile(DefaultLangDir + s, Language);
end;

procedure MakeTranslationFile(const FileName, Language: AnsiString);
var
  Dst: TTranslateStringList;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then begin
    Dst := TTranslateStringList.Create;
    try
      Dst.Values[sLanguageIDName]:= Language;
      with LRSTranslator as TResTranslator do
        Dst.Merge(Strings as TTranslateStringList, true);
      ForceDirectories(ExtractFilePath(FileName));
      Dst.SaveToFile(FileName);
    finally
      Dst.Free;
    end;
  end;
end;

procedure SupplementTranslationFile(const FileName: AnsiString);
var
  Dst: TTranslateStringList;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then begin
    Dst := TTranslateStringList.Create(FileName);
    try
      with LRSTranslator as TResTranslator do
        Dst.Merge(Strings as TTranslateStringList, true);
      Dst.SaveToFile(FileName);
    finally
      Dst.Free;
    end;
  end;
end;

procedure SupplementTranslationFiles(const TranslationFilesPath: AnsiString);
var
  Sl: TStringList;
  i: integer;
  s: string;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then begin
    Sl := GetAvailableTranslations(TranslationFilesPath);
    with Sl do
      for i := 0 to Count - 1 do
        SupplementTranslationFile(IncludeTrailingPathDelimiter(TranslationFilesPath) + ValueFromIndex[i]);
    // Supplement template file
    s:=IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(ParamStr(0)) + '.template';
    if FileExists(s) then
      SupplementTranslationFile(s);
  end;
end;

function LoadDefaultTranslationFile(const OnTranslate: TTranslateStringEvent): TFileName;
begin
  Result := LoadDefaultTranslationFile(DefaultLangDir, OnTranslate);
end;

function LoadDefaultTranslationFile(const TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
var
  lLang, sLang: string;
begin
  LCLGetLanguageIDs(lLang, sLang);
  Result := IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(ParamStr(0))+ '.' + AnsiLowerCase(sLang);
  Result := LoadTranslationFile(Result, OnTranslate);
end;

function LoadLanguageTranslation(const Language: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
begin
  Result := LoadLanguageTranslation(Language, DefaultLangDir, OnTranslate);
end;

function LoadLanguageTranslation(const Language, TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
var
  Sl: TStringList;
begin
  Sl:= GetAvailableTranslations(TranslationFilesPath);
  Result:= GetTranslationFileName(Language, Sl);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(TranslationFilesPath) + Result;
  if FileExists(Result) then
    LoadTranslationFile(Result, OnTranslate);
end;

function GetTranslationFileName(const Language: AnsiString; AvailableTranslations: TStringList): AnsiString;
var
  i: integer;
  aName, aValue: string;
begin
  Result := '';
  if Assigned(AvailableTranslations) then
    with AvailableTranslations do
      for i := 0 to Count - 1 do begin
        GetNameValue(i, aName, aValue);
        if AnsiSameText(AnsiDequotedStr(Language, QuoteChar), AnsiDequotedStr(aName, QuoteChar)) then begin
          Result:= AnsiDequotedStr(aValue, QuoteChar);
          Break;
        end;
      end;
end;

procedure SaveTranslationFile; overload;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      if Modified then
        SaveFile;
end;

procedure SaveTranslationFile(const FileName: AnsiString); overload;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      if Modified then
        SaveFile(FileName);
end;

function TranslateString(const Value: AnsiString): AnsiString;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      result := InternalTranslateString(Value)
  else
    result := Value;
end;

{ TTranslateStringList }

function TTranslateStringList.CorrectGetValue(const Name: string): string;
var
  Index: integer;
  offset: integer;
begin
  Index := IndexOfName(Name, offset);
  if Index >= 0  then
    Result := Copy(Strings[Index], offset, MaxInt)
  else
    result := '';
end;

function TTranslateStringList.CorrectGetName(Index: integer): string;
var
  Offset: integer;
  s: string;
begin
  CheckSpecialChars;
  Result := '';
  s := Strings[Index];
  Offset := ScanQuotSep(PChar(s));
  if (Offset > 0) then
    Result := NormaliseQuotedStr(LeftStr(s, offset));
end;

function TTranslateStringList.ScanQuotSep(P: PChar): integer;
var
 i, len: integer;
 QuoteCount: integer;
begin
  result := 0;
  QuoteCount := 0;
  i := 0;
  len:=strlen(P);
  while (i < len) and (result = 0) do begin
    if P[i] = QuoteChar then
      inc(QuoteCount)
    else if (P[i] = NameValueSeparator) and not odd(QuoteCount) then
      result := i;
    inc(i);
  end;
end;

function TTranslateStringList.DoCompareText(const s1, s2: string): PtrInt;
begin
 if CaseSensitive then
   result:=AnsiCompareText(s1,s2)
 else
   result:=AnsiCompareText(UTF8UpperCase(s1),UTF8UpperCase(s2));
end;

constructor TTranslateStringList.Create(const FileName: string);
begin
  inherited Create;
  CheckSpecialChars;
  LoadFromFile(FileName);
end;

function TTranslateStringList.NormaliseQuotedStr(const S: string): string;
begin
  if not HasSeparator(S, NameValueSeparator) then
    Result := AnsiDequotedStr(S, QuoteChar)
  else if not IsQuoted(S, QuoteChar) then
    Result := AnsiQuotedStr(S, QuoteChar)
  else
    Result := S;
end;

function TTranslateStringList.IndexOfName(const Name: string; var Offset: integer): integer;
var
  s, n: string;
begin
  CheckSpecialChars;
  result := 0;
  n:=NormaliseQuotedStr(Name);
  while (result < Count) do begin
    s:=Strings[result];
    Offset := ScanQuotSep(PChar(s));
    if (Offset > 0) and AnsiSameText(n, Copy(s, 1, Offset)) then begin
      inc(Offset, 2);
      exit;
    end;
    inc(result);
  end;
  result := -1;
end;

function TTranslateStringList.IndexOfName(const Name: string): integer;
var
  i: integer;
begin
  Result:=IndexOfName(Name, i);
end;

procedure TTranslateStringList.LoadFromFile(const FileName: string);
var
  FS: TFileStream;
  buff: array[1..3] of char;
  i, j: integer;
  s: string;
begin
  FS:= TFileStream.Create(FileName, fmOpenRead);
  try
    // Skip UTF8 header
    buff := '';
    FS.Read(buff, SizeOf(UTF8FileHeader));
    if buff <> UTF8FileHeader then
      FS.Position:=0;
    LoadFromStream(FS);
  finally
    FS.Free;
  end;

  i:=IndexOf(LineSeparator);
  if i >= 0 then
    Delete(i);

  // Normalize quotations
  for i:=0 to Count - 1 do begin
    s:=Strings[i];
    j:=ScanQuotSep(PChar(s));
    if j > 0 then
      Strings[i]:=NormaliseQuotedStr(Copy(s, 1, j)) + NameValueSeparator + NormaliseQuotedStr(Copy(s, j + 2, MaxInt));
  end;
end;

procedure TTranslateStringList.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  ForceDirectories(ExtractFilePath(FileName));
  FS := TFileStream.Create(FileName, fmCreate);
  try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TTranslateStringList.Merge(Source: TTranslateStringList; const NamesOnly: boolean = false);
var
  i: integer;
begin
  CheckSpecialChars;
  Source.Sort;
  for i := 0 to Source.Count - 1 do
    if (Source.CNames[i] <> '') and (IndexOfName(Source.CNames[i]) = -1) then begin
      if NamesOnly then
        Append(Source.CNames[i] + NameValueSeparator + Source.CNames[i])
      else
        Append(Source.Strings[i]);
    end;
end;

{ TResTranslator }

constructor TResTranslator.Create(TranslationFile: AnsiString);
begin
  inherited Create;
  FTranslationFile := TranslationFile;
  FIgnoreDelimiters := [wdIgnoreTrailing];
  FWordDelims := ['.', ',', ':'];

  FStrResLst  := TTranslateStringList.Create;
  with FStrResLst do begin
    Duplicates := dupIgnore;
    CaseSensitive := False;
    CheckSpecialChars;
    if FileExists(FTranslationFile) then begin
      LoadFromFile(FTranslationFile);
      FTranslationLanguage := AnsiDequotedStr(CValues[AnsiQuotedStr(sLanguageIDName, QuoteChar)], QuoteChar);
    end;
  end;

  FAddedStrings := TTranslateStringList.Create;
  with FAddedStrings do begin
    Sorted     := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
    CheckSpecialChars;
  end;

  FStrings := TTranslateStringList.Create;
  FStrings.Assign(FStrResLst);
end;

destructor TResTranslator.Destroy;
begin
  FStrResLst.Free;
  FAddedStrings.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TResTranslator.InternalTranslateString(const Value: AnsiString): AnsiString;

  function IsAlpha(Ch: char): boolean; inline;
  begin
    Result := Ch in ['A'..'Z', 'a'..'z'];
  end;

  function HasAlpha: boolean;
  var
    i: integer;
  begin
    Result := False;
    i      := 1;
    while not Result and (i <= Length(Value)) do begin
      Result := IsAlpha(Value[i]);
      Inc(i);
    end;
  end;

var
  ClearValue: AnsiString;
  Original, s, n: AnsiString;
begin
  Original := Value;
  ClearValue := StringReplace(AdjustLineBreaks(Value), LineEnding, '~', [rfReplaceAll]);
  Result := ClearValue;

  if wdIgnoreLeading in IgnoreDelimiters then
    RemoveLeadingChars(ClearValue, FWordDelims);

  if wdIgnoreTrailing in IgnoreDelimiters then
    RemoveTrailingChars(ClearValue, FWordDelims);
  if HasAlpha then
  begin
    with FStrResLst do begin
      if HasSeparator(ClearValue, NameValueSeparator) then
        n := AnsiQuotedStr(ClearValue, QuoteChar)
      else
        n := ClearValue;

      s:=CValues[n];
      if (s = '') then begin
        with FAddedStrings do
          Append(n + NameValueSeparator + n);
        with FStrings do
          Append(n + NameValueSeparator + n);
        FModified := True;
        Result := Original;
      end
      else begin
        Result := StringReplace(Result, ClearValue, AnsiDequotedStr(s, QuoteChar), [rfReplaceAll]);
        Result := StringReplace(Result, '~', LineEnding, [rfReplaceAll]);
      end;
    end;
  end;
end;

procedure TResTranslator.SetIgnoreDelimiters(const AValue: TWordDelimitersOptions);
begin
  if FIgnoreDelimiters = AValue then
    exit;
  FIgnoreDelimiters := AValue;
end;

function TResTranslator.GetAddedStrings: TStringList;
begin
//convert TTranslateStringList to TStringList
  Result := FAddedStrings;
end;

function TResTranslator.GetStrings: TStringList;
begin
//convert TTranslateStringList to TStringList
  Result := FStrings;
end;

procedure TResTranslator.SetOnTranslateString(const AValue: TTranslateStringEvent);
begin
  if FOnTranslateString = AValue then
    exit;
  FOnTranslateString := AValue;
end;

procedure TResTranslator.SetWordDelims(const AValue: TSysCharset);
begin
  if FWordDelims = AValue then
    exit;
  FWordDelims := AValue;
end;

procedure TResTranslator.TranslateStringProperty(Sender: TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  Accept: boolean;
  ResourceName: AnsiString;
  OwnerName: AnsiString;
begin
  if Sender is TReader and Assigned(TReader(Sender).Owner) then
    OwnerName := TReader(Sender).Owner.GetNamePath;

  if Instance.InheritsFrom(TForm) then
    ResourceName := OwnerName + '.' + PropInfo^.Name
  else
    ResourceName := OwnerName + '.' + Instance.GetNamePath + '.' + PropInfo^.Name;

  Accept := True;

  if Assigned(OnTranslateString) then
    OnTranslateString(Self, ResourceName, Accept);

  if (PropInfo^.Name = 'Caption') and (Instance.GetNamePath = Content) then
    Accept:=False;

  if Accept then
    Content := InternalTranslateString(Content);
end;

procedure TResTranslator.SaveFile;
begin
  SaveTranslationFile(FTranslationFile);
end;

procedure TResTranslator.SaveFile(const aFileName: string);
var
  FS: TFileStream;
begin
  ForceDirectories(ExtractFilePath(aFileName));
  FS := TFileStream.Create(aFileName, fmCreate);
  try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    FStrResLst.SaveToStream(FS);
    FAddedStrings.SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

finalization
  FreeAndNil(LRSTranslator);

end.
