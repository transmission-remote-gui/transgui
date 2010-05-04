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

  { TCorrectedStringList }

  TCorrectedStringList = class(TStringList)
  private
    function CorrectGetValue(const Name: string): string;
    function ScanQuotSep(P: PChar):integer;
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  public
    constructor Create(const FileName: string); overload;
    function IndexOfName(const Name: string; var Offset: integer): integer;
    function IndexOfName(const Name: string): integer; override;
    procedure LoadFromFile(const FileName: string); override;
    property CValues[const Name: string]: string read CorrectGetValue;
  end;

  { TResTranslator }

  TResTranslator = class(TAbstractTranslator)
  private
    FIgnoreDelimiters: TWordDelimitersOptions;
    FOnTranslateString: TTranslateStringEvent;
    FStrResLst:    TCorrectedStringList;
    FAddedStrings: TCorrectedStringList;
    FTranslationFile: string;
    FModified:     boolean;
    FTranslationLanguage: AnsiString;
    FWordDelims:   TSysCharset;
    function GetAddedStrings: TStringList;
    procedure SetIgnoreDelimiters(const AValue: TWordDelimitersOptions);
    procedure SetOnTranslateString(const AValue: TTranslateStringEvent);
    procedure SetWordDelims(const AValue: TSysCharset);
    function InternalTranslateString(Value: AnsiString): AnsiString;
  public
    constructor Create(TranslationFile: AnsiString);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content: string); override;
    procedure SaveFile; overload;
    procedure SaveFile(const aFileName: string); overload;
    property Modified: boolean Read FModified;
    property AddedStrings: TStringList Read GetAddedStrings;
    property IgnoreDelimiters: TWordDelimitersOptions Read FIgnoreDelimiters Write SetIgnoreDelimiters;
    property WordDelims: TSysCharset Read FWordDelims Write SetWordDelims;
    property TranslationLanguage: AnsiString read FTranslationLanguage;
    property OnTranslateString: TTranslateStringEvent Read FOnTranslateString Write SetOnTranslateString;
  end;

function LoadTranslationFile(const TranslationFile: AnsiString; const OnTranslate: TTranslateStringEvent = nil): AnsiString;
procedure SaveTranslationFile; overload;
procedure SaveTranslationFile(const aFileName: AnsiString); overload;
function LoadDefaultTranslationFile(const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadDefaultTranslationFile(const TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadLanguageTranslation(const Language: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function LoadLanguageTranslation(const Language, TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent = nil): TFileName;
function TranslateString(const Value: AnsiString): AnsiString;
function ExtractLangName(const aFileName: TFilename): AnsiString;
function GetAvailableTranslations: TStringList;
function GetAvailableTranslations(const aSearchPath: AnsiString): TStringList;
function GetTranslationFileName(const Language: string; AvailableTranslations: TStringList): string;

const
  sLanguageIDName = 'TranslationLanguage';
  sDefautTranslationDir = 'lang';

implementation

uses
  Forms;

{ procedures and functions }

function ExtractLangName(const aFileName: TFilename): AnsiString;
begin
  with TCorrectedStringList.Create(aFileName) do
    try
      Result := AnsiDequotedStr(CValues[AnsiQuotedStr(sLanguageIDName, QuoteChar)], QuoteChar);
    finally
      Free;
    end;
end;

function GetAvailableTranslations: TStringList;
begin
  Result:= GetAvailableTranslations(ExtractFilePath(ParamStr(0)) + 'lang' + DirectorySeparator);
end;

function GetAvailableTranslations(const aSearchPath: AnsiString): TStringList;
var
  Sr: TSearchRec;
  LangName: AnsiString;
begin
  Result:= TStringList.Create;
  if FindFirstUTF8(IncludeTrailingPathDelimiter(aSearchPath) + '*', faArchive or faReadOnly, Sr) = 0 then
    with Result do begin
      NameValueSeparator:= '=';
      QuoteChar:= '"';
      repeat
        LangName:= ExtractLangName(IncludeTrailingPathDelimiter(ExtractFilePath(aSearchPath)) + Sr.Name);
        if LangName <> '' then
          Add(LangName + NameValueSeparator + Sr.Name);
      until FindNextUTF8(Sr) <> 0;
      FindCloseUTF8(Sr);
    end;
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

function LoadDefaultTranslationFile(const OnTranslate: TTranslateStringEvent): TFileName;
begin
  Result := LoadDefaultTranslationFile(ExtractFilePath(ParamStr(0)) + sDefautTranslationDir + DirectorySeparator, OnTranslate);
end;

function LoadDefaultTranslationFile(const TranslationFilesPath: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
var
  lLang, sLang: string;
begin
  LCLGetLanguageIDs(lLang, sLang);
  Result := IncludeTrailingPathDelimiter(TranslationFilesPath) + ExtractFileNameOnly(ParamStr(0))+ '.' + AnsiLowerCase(sLang);
  LoadTranslationFile(Result, OnTranslate);
end;

function LoadLanguageTranslation(const Language: AnsiString; const OnTranslate: TTranslateStringEvent): TFileName;
begin
  Result := LoadLanguageTranslation(Language, ExtractFilePath(ParamStr(0)) + sDefautTranslationDir + DirectorySeparator, OnTranslate);
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

function GetTranslationFileName(const Language: string; AvailableTranslations: TStringList): string;
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

procedure SaveTranslationFile(const aFileName: AnsiString); overload;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      if Modified then
        SaveFile(aFileName);
end;

function TranslateString(const Value: AnsiString): AnsiString;
begin
  if Assigned(LRSTranslator) and (LRSTranslator is TResTranslator) then
    with LRSTranslator as TResTranslator do
      result := InternalTranslateString(Value);
end;

{ TCorrectedStringList }

function TCorrectedStringList.CorrectGetValue(const Name: string): string;
var
  Index: integer;
  offset: integer;
begin
  result := '';
  Index := IndexOfName(Name, offset);
  if Index >= 0  then
    result := Copy(Strings[Index], offset, Length(Strings[Index]));
end;

function TCorrectedStringList.ScanQuotSep(P: PChar): integer;
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

function TCorrectedStringList.DoCompareText(const s1, s2: string): PtrInt;
begin
 if CaseSensitive then
   result:=AnsiCompareText(s1,s2)
 else
   result:=AnsiCompareText(UTF8UpperCase(s1),UTF8UpperCase(s2));
end;

constructor TCorrectedStringList.Create(const FileName: string);
begin
  inherited Create;
  CheckSpecialChars;
  LoadFromFile(FileName);
end;

function TCorrectedStringList.IndexOfName(const Name: string; var Offset: integer): integer;
var
  P: PChar;
begin
  CheckSpecialChars;
  result := 0;
  while (result < Count) do
    begin
    P := PChar(Strings[result]);
{ TODO : Name may by quoted!}
    Offset := ScanQuotSep(P);
    if (Offset > 0) and (AnsiStrLIComp(P, PChar(Name), Offset) = 0) then begin
      inc(Offset, 2);
      exit;
    end;
    inc(result);
    end;
  result := -1;
end;

function TCorrectedStringList.IndexOfName(const Name: string): integer;
var
  i: integer;
begin
  Result:=IndexOfName(Name, i);
end;

procedure TCorrectedStringList.LoadFromFile(const FileName: string);
var
  FS: TFileStream;
  buff: array[1..3] of char;
begin
  FS:= TFileStream.Create(UTF8ToSys(FileName), fmOpenRead);
  try
    // Skip UTF8 headers
    buff := '';
    FS.Read(buff, SizeOf(UTF8FileHeader));
    if buff <> UTF8FileHeader then
      FS.Position:=0;
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

{ TResTranslator }

constructor TResTranslator.Create(TranslationFile: AnsiString);
begin
  inherited Create;
  FTranslationFile := TranslationFile;
  FIgnoreDelimiters := [wdIgnoreTrailing];
  FWordDelims := ['.', ',', ':'];

  FStrResLst  := TCorrectedStringList.Create;
  with FStrResLst do begin
    Duplicates := dupIgnore;
    CaseSensitive := False;
    CheckSpecialChars;
    if FileExists(FTranslationFile) then
      LoadFromFile(FTranslationFile);
    FTranslationLanguage := AnsiDequotedStr(CValues[AnsiQuotedStr(sLanguageIDName, QuoteChar)], QuoteChar);
  end;

  FAddedStrings := TCorrectedStringList.Create;
  with FAddedStrings do begin
    Sorted     := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
    CheckSpecialChars;
  end;
end;

destructor TResTranslator.Destroy;
begin
  FStrResLst.Free;
  FAddedStrings.Free;
  inherited Destroy;
end;

function TResTranslator.InternalTranslateString(Value: AnsiString): AnsiString;

  function IsAlpha(Ch: char): boolean;
  inline;
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
begin
  ClearValue := StringReplace(AdjustLineBreaks(Value), LineEnding, '~', [rfReplaceAll]);

  Result := ClearValue;

  if wdIgnoreLeading in IgnoreDelimiters then
    RemoveLeadingChars(ClearValue, FWordDelims);

  if wdIgnoreTrailing in IgnoreDelimiters then
    RemoveTrailingChars(ClearValue, FWordDelims);
  if HasAlpha then
  begin
    with FStrResLst do begin
      Value := AnsiQuotedStr(ClearValue, QuoteChar);
      if (CValues[Value] = '') then begin
        with FAddedStrings do
          Append(Value + NameValueSeparator + Value);
        FModified := True;
      end
      else begin
        Result := StringReplace(Result, ClearValue, AnsiDequotedStr(CValues[Value], QuoteChar), [rfReplaceAll]);
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
  Result := FAddedStrings;
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
const
  LineSeparator = '###################';
var
  FS: TFileStream;
begin
  ForceDirectories(ExtractFilePath(aFileName));
  FS := TFileStream.Create(aFileName, fmCreate);
  try
    FS.WriteBuffer(UTF8FileHeader, SizeOf(UTF8FileHeader));
    with FStrResLst do begin
      while IndexOf(LineSeparator) >= 0 do
        Delete(IndexOf(LineSeparator));
      SaveToStream(FS);
      if (Count > 0) and (FAddedStrings.Count > 0) then begin
        FS.WriteBuffer(LineSeparator, Length(LineSeparator));
        FS.WriteBuffer(string(LineEnding)[1], Length(LineEnding));
      end;
    end;
    FAddedStrings.SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

finalization
  FreeAndNil(LRSTranslator);

end.
