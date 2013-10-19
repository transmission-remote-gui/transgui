{************************************************************
   Copyright (c) 2010 Alex Cherednichenko, aka Alex7Che.
   Copyright (c) 2011-2013 Yury Sidorov.

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

  TTranslateStringOption = (tsoNew, tsoUsed, tsoExternal);
  TTranslateStringOptions = set of TTranslateStringOption;

  { TTranslateStringList }

  TTranslateStringList = class(TStringList)
  private
    function CorrectGetName(Index: integer): string;
    function CorrectGetValue(const Name: string): string;
    function GetOptions(Index: integer): TTranslateStringOptions;
    function NormaliseQuotedStr(const S: string): string;
    function ScanQuotSep(P: PChar):integer;
    procedure SetOptions(Index: integer; const AValue: TTranslateStringOptions);
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
    property Options[Index: integer]: TTranslateStringOptions read GetOptions write SetOptions;
  end;

  { TResTranslator }

  TResTranslator = class(TAbstractTranslator)
  private
    FIgnoreDelimiters: TWordDelimitersOptions;
    FOnTranslateString: TTranslateStringEvent;
    FStrResLst:    TTranslateStringList;
    FTranslationFile: string;
    FModified:     boolean;
    FTranslationLanguage: AnsiString;
    FWordDelims:   TSysCharset;
    function GetStrings: TStringList;
    procedure SetIgnoreDelimiters(const AValue: TWordDelimitersOptions);
    procedure SetOnTranslateString(const AValue: TTranslateStringEvent);
    procedure SetWordDelims(const AValue: TSysCharset);
    function InternalTranslateString(const Value: AnsiString; IsExternal: boolean = False): AnsiString;
  public
    constructor Create(TranslationFile: AnsiString);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content: string); override;
    procedure SaveFile; overload;
    procedure SaveFile(const aFileName: string); overload;
    property Modified: boolean Read FModified;
    property Strings: TStringList Read GetStrings;
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
function TranslateString(const Value: AnsiString; IsExternal: boolean = False): AnsiString;
function ExtractLangName(const FileName: TFilename): AnsiString;
function GetAvailableTranslations: TStringList;
function GetAvailableTranslations(const SearchPath: AnsiString): TStringList;
function GetTranslationFileName(const Language: AnsiString; AvailableTranslations: TStringList): AnsiString;
function DefaultLangDir: AnsiString;
function IsTranslationFileValid(const TranslationFile: AnsiString): boolean;

const
  sLanguageIDName = 'TranslationLanguage';

implementation

uses
  Forms, utils;

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
  LangName, s: AnsiString;
begin
  Result:= TStringList.Create;
  if FindFirstUTF8(IncludeTrailingPathDelimiter(SearchPath) + '*', faArchive or faReadOnly, Sr) = 0 then
    with Result do begin
      NameValueSeparator:= '=';
      QuoteChar:= '"';
      repeat
        if ExtractFileExt(Sr.Name) = '.template' then
          continue;
        s:=IncludeTrailingPathDelimiter(ExtractFilePath(SearchPath)) + Sr.Name;
        if IsTranslationFileValid(s) then begin
          LangName:= ExtractLangName(s);
          if LangName <> '' then
            Add(LangName + NameValueSeparator + Sr.Name);
        end;
      until FindNextUTF8(Sr) <> 0;
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
    Result:=FindFirstUtf8(dir + ExtractFileNameOnly(ParamStrUtf8(0)) + '.*', faAnyFile, sr) = 0;
    FindClose(sr);
  end;

var
  s: string;
{$endif unix}
begin
  if FDefaultLangDir = '' then begin
    FDefaultLangDir:=ExtractFilePath(ParamStrUtf8(0)) + 'lang' + DirectorySeparator;
{$ifdef unix}
    if not _IsLangDir(FDefaultLangDir) then begin
      s:='/usr/share/' + ExtractFileNameOnly(ParamStrUtf8(0)) + '/lang/';
      if _IsLangDir(s) then
        FDefaultLangDir:=s
      else begin
        s:='/usr/local/share/' + ExtractFileNameOnly(ParamStrUtf8(0)) + '/lang/';
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
  s:=ExtractFileNameOnly(ParamStrUtf8(0));
  if (sLang <> '') and not FileExistsUTF8(DefaultLangDir + s + '.' + sLang) then
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
    s:=IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(ParamStrUtf8(0)) + '.template';
    if FileExistsUTF8(s) then
      SupplementTranslationFile(s);
  end;
end;

const
  InvalidLangExt: array[1..6] of string = ('ua', 'by', 'cn', 'cz', 'se', 'tw');

function IsTranslationFileValid(const TranslationFile: AnsiString): boolean;
var
  s: string;
  i: integer;
begin
  Result:=FileExistsUTF8(TranslationFile);
  if not Result then
    exit;
  s:=LowerCase(ExtractFileExt(TranslationFile));
  Delete(s, 1, 1);
  for i:=Low(InvalidLangExt) to High(InvalidLangExt) do
    if s = InvalidLangExt[i] then begin
      Result:=False;
      exit;
    end;
end;

function LoadDefaultTranslationFile(const OnTranslate: TTranslateStringEvent): TFileName;
begin
  Result := LoadDefaultTranslationFile(DefaultLangDir, OnTranslate);
end;

function LoadDefaultTranslationFile(const TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
var
  lLang, sLang, s: string;
  i: integer;
begin
  LCLGetLanguageIDs(lLang, sLang);
  lLang:=LowerCase(lLang);
  sLang:=LowerCase(sLang);
{$ifdef windows}
  if sLang = 'ch' then begin
    sLang:='zh';
    lLang:=StringReplace(lLang, 'ch_', 'zh_', []);
  end;
{$endif windows}
  i:=Pos('.', lLang);
  if i > 0 then
    SetLength(lLang, i - 1);
  s:=IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(ParamStrUtf8(0))+ '.';
  Result := s + lLang;
  // First check full language name (uk_ua)
  if not IsTranslationFileValid(Result) then begin
    Result := s + sLang;
    // Check fallback language name (uk)
    if not IsTranslationFileValid(Result) then begin
      // Finally use country name (ua)
      i:=Pos('_', lLang);
      if i > 0 then
        lLang:=Copy(lLang, i + 1, MaxInt);
      Result := s + lLang;
      if not IsTranslationFileValid(Result) then begin
        Result:='';
        exit;
      end;
    end;
  end;
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
  if FileExistsUTF8(Result) then
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

function TranslateString(const Value: AnsiString; IsExternal: boolean): AnsiString;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      result := InternalTranslateString(Value, IsExternal)
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
  if Index >= 0  then begin
    Result := Copy(Strings[Index], offset, MaxInt);
    Options[Index]:=Options[Index] + [tsoUsed];
  end
  else
    result := '';
end;

function TTranslateStringList.GetOptions(Index: integer): TTranslateStringOptions;
begin
  Result:=TTranslateStringOptions(cardinal(ptruint(Objects[Index])));
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

procedure TTranslateStringList.SetOptions(Index: integer; const AValue: TTranslateStringOptions);
begin
  Objects[Index]:=TObject(ptruint(cardinal(AValue)));
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
    if (Offset > 0) and (n = Copy(s, 1, Offset)) then begin
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
  FS: TFileStreamUTF8;
  buff: array[1..3] of char;
  i, j, k: integer;
  s, esep: string;
begin
  FS:= TFileStreamUTF8.Create(FileName, fmOpenRead);
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
  esep:=NameValueSeparator + NameValueSeparator;
  for i:=0 to Count - 1 do begin
    s:=Strings[i];
    j:=ScanQuotSep(PChar(s));
    if j > 0 then begin
      k:=j + 2;
      if Copy(s, j + 1, 2) = esep then begin
        Options[i]:=[tsoExternal];
        Inc(k);
      end;
      Strings[i]:=NormaliseQuotedStr(Copy(s, 1, j)) + NameValueSeparator + NormaliseQuotedStr(Copy(s, k, MaxInt));
    end;
  end;
end;

procedure TTranslateStringList.SaveToFile(const FileName: string);
var
  FS: TFileStreamUTF8;
  i, j: integer;
  s, esep: string;
begin
  ForceDirectories(ExtractFilePath(FileName));
  FS := TFileStreamUTF8.Create(FileName, fmCreate);
  try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    esep:=NameValueSeparator + NameValueSeparator;
    for i:=0 to Count - 1 do begin
      s:=Strings[i];
      if tsoExternal in Options[i] then begin
        j:=ScanQuotSep(PChar(s));
        if j > 0 then
          s:=NormaliseQuotedStr(Copy(s, 1, j)) + esep + NormaliseQuotedStr(Copy(s, j + 2, MaxInt));
      end;
      if s <> '' then
        FS.WriteBuffer(s[1], Length(s));
      s:=LineEnding;
      FS.WriteBuffer(s[1], Length(s));
    end;
  finally
    FS.Free;
  end;
end;

procedure TTranslateStringList.Merge(Source: TTranslateStringList; const NamesOnly: boolean = false);
var
  i, j: integer;
  n: string;
begin
  CheckSpecialChars;
  Source.Sort;
  for i:=0 to Count - 1 do
    Options[i]:=[];
  for i:=0 to Source.Count - 1 do begin
    if Source.Options[i]*[tsoUsed, tsoExternal] = [] then
      continue;
    n:=Source.CNames[i];
    if n <> '' then begin
      j:=IndexOfName(n);
      if j < 0 then begin
        // New string
        if NamesOnly then
          j:=Add(n + NameValueSeparator + n)
        else
          j:=Add(Source.Strings[i]);
      end;
      Options[j]:=Source.Options[i] + [tsoUsed];
    end;
  end;
  // Delete unused strings
  i:=0;
  while i < Count do begin
    n:=CNames[i];
    if (Options[i] = []) and (n <> '') and (CompareText(n, sLanguageIDName) <> 0) then
      Delete(i)
    else
      Inc(i);
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
    if FileExistsUTF8(FTranslationFile) then begin
      LoadFromFile(FTranslationFile);
      FTranslationLanguage := AnsiDequotedStr(CValues[AnsiQuotedStr(sLanguageIDName, QuoteChar)], QuoteChar);
    end;
  end;
end;

destructor TResTranslator.Destroy;
begin
  FStrResLst.Free;
  inherited Destroy;
end;

function TResTranslator.InternalTranslateString(const Value: AnsiString; IsExternal: boolean): AnsiString;

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
  i: integer;
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
        i:=Add(n + NameValueSeparator + n);
        Options[i]:=[tsoNew, tsoUsed];
        FModified := True;
        Result := Original;
      end
      else begin
        Result := StringReplace(Result, ClearValue, AnsiDequotedStr(s, QuoteChar), [rfReplaceAll]);
        Result := StringReplace(Result, '~', LineEnding, [rfReplaceAll]);
      end;
      if IsExternal then begin
        i:=IndexOfName(n);
        if i >= 0 then
          Options[i]:=Options[i] + [tsoExternal];
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

function TResTranslator.GetStrings: TStringList;
begin
  Result := FStrResLst;
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
    Accept:=False
  else
    if PropInfo^.Name = 'Name' then
      Accept:=False;

  if Accept then
    Content := InternalTranslateString(Content);
end;

procedure TResTranslator.SaveFile;
begin
  SaveTranslationFile(FTranslationFile);
end;

procedure TResTranslator.SaveFile(const aFileName: string);
begin
  FStrResLst.SaveToFile(aFileName);
end;

finalization
  FreeAndNil(LRSTranslator);

end.
