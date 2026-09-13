program CopyMagnetTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, Variants, fpjson, jsonparser;

const
  CF_Text = 1;
  ExternalLink = 'magnet:?xt=urn:btih:external';

type
  EClipboardWrite = class(Exception);
  EClipboardRead = class(Exception);
  ERpcFailure = class(Exception);
  TWriteMode = (wmSuccess, wmFailBeforeWrite, wmFailAfterWrite, wmFailAfterReplacement);
  TReadMode = (rmSuccess, rmNoFormat, rmFailFormat, rmFailRead);
  TOutcome = (ocCopy, ocInvalid, ocNil, ocNoSelection, ocClipboardFailure, ocRpcFailure);

  TMainForm = class(TComponent)
  public
    FLastClipboardLink: string;
    FLinksFromClipboard: Boolean;
    FCheckingClipboardLink: Boolean;
    FPendingClipboardTorrents: TStringList;
    SelectedTorrents: Variant;
    StatusCount: Integer;
    LastStatus: string;
    function GetSelectedTorrents: Variant;
    procedure CheckStatus(Fatal: Boolean; const AStatus: string = '');
    procedure MenuItem101Click(Sender: TObject);
    procedure CheckClipboardLink;
  end;

  TTestRpc = class
  public
    Response: string;
    Calls: Integer;
    Fail: Boolean;
    function SendRequest(req: TJSONObject): TJSONObject;
  end;

  TTestClipboard = class
  private
    function GetAsText: string;
    procedure SetAsText(const Value: string);
  public
    Text: string;
    Calls, ReadCalls, FormatCalls: Integer;
    WriteMode: TWriteMode;
    ReadMode: TReadMode;
    MonitorDuringWrite: Boolean;
    RaisedException: Pointer;
    function HasFormat(AFormat: Integer): Boolean;
    property AsText: string read GetAsText write SetAsText;
  end;

var
  MainForm: TMainForm;
  RpcObj: TTestRpc;
  Clipboard: TTestClipboard;
  TestsRun: Integer;

function IsHash(Hash: String): boolean;
begin
  // Hash normalization is outside this focused test; all monitored fixtures are magnet URIs.
  Result:=False;
end;

function TMainForm.GetSelectedTorrents: Variant;
begin
  Result:=SelectedTorrents;
end;

procedure TMainForm.CheckStatus(Fatal: Boolean; const AStatus: string);
begin
  Assert(not Fatal, 'copy-magnet errors must remain nonfatal');
  Inc(StatusCount);
  LastStatus:=AStatus;
end;

function TTestRpc.SendRequest(req: TJSONObject): TJSONObject;
var
  args: TJSONObject;
  ids, fields: TJSONArray;
begin
  Inc(Calls);
  Assert(req.Count = 2, 'request shape changed');
  Assert(req.Strings['method'] = 'torrent-get', 'RPC method changed');
  args:=req.Objects['arguments'];
  Assert(args.Count = 2, 'request argument shape changed');
  ids:=args.Arrays['ids'];
  Assert((ids.Count = 2) and (ids.Integers[0] = 7) and (ids.Integers[1] = 42),
    'selected torrent IDs changed');
  fields:=args.Arrays['fields'];
  Assert((fields.Count = 1) and (fields.Strings[0] = 'magnetLink'),
    'requested fields changed');
  if Fail then
    raise ERpcFailure.Create('injected RPC failure');
  if Response = '' then
    Result:=nil
  else
    Result:=GetJSON(Response) as TJSONObject;
end;

function TTestClipboard.HasFormat(AFormat: Integer): Boolean;
begin
  Inc(FormatCalls);
  Assert(AFormat = CF_Text, 'unexpected clipboard format');
  if ReadMode = rmFailFormat then
    raise EClipboardRead.Create('injected format query failure');
  Result:=ReadMode <> rmNoFormat;
end;

function TTestClipboard.GetAsText: string;
begin
  Inc(ReadCalls);
  if ReadMode = rmFailRead then
    raise EClipboardRead.Create('injected clipboard read failure');
  Result:=Text;
end;

procedure TTestClipboard.SetAsText(const Value: string);
var
  Error: EClipboardWrite;
begin
  Inc(Calls);
  Assert(MainForm.FLastClipboardLink = Value, 'marker was not set before the write');
  case WriteMode of
    wmSuccess, wmFailAfterWrite: Text:=Value;
    wmFailAfterReplacement: Text:=ExternalLink;
  end;
  if MonitorDuringWrite then
    MainForm.CheckClipboardLink;
  if WriteMode <> wmSuccess then begin
    Error:=EClipboardWrite.Create('injected clipboard failure');
    RaisedException:=Pointer(Error);
    raise Error;
  end;
end;

// Extracted verbatim: the handler, monitor, safe reader and normalization helpers.
{$I copy_magnet_handler.inc}

procedure RunCase(const Name, Response, Expected: string; Outcome: TOutcome;
  const PreviousMarker: string = 'previously observed text';
  WriteMode: TWriteMode = wmSuccess; ReadMode: TReadMode = rmSuccess;
  MonitorDuringWrite: Boolean = False);
var
  Raised, ConfirmedWrite: Boolean;
begin
  MainForm.FLastClipboardLink:=PreviousMarker;
  MainForm.FLinksFromClipboard:=True;
  MainForm.FCheckingClipboardLink:=False;
  MainForm.FPendingClipboardTorrents.Clear;
  MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
  if Outcome = ocNoSelection then
    MainForm.SelectedTorrents:=Unassigned;
  MainForm.StatusCount:=0;
  MainForm.LastStatus:='';
  RpcObj.Response:=Response;
  RpcObj.Calls:=0;
  RpcObj.Fail:=Outcome = ocRpcFailure;
  Clipboard.Text:='original clipboard contents';
  Clipboard.Calls:=0;
  Clipboard.ReadCalls:=0;
  Clipboard.FormatCalls:=0;
  Clipboard.WriteMode:=WriteMode;
  Clipboard.ReadMode:=ReadMode;
  Clipboard.MonitorDuringWrite:=MonitorDuringWrite;
  Clipboard.RaisedException:=nil;
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do begin
      Assert(Outcome = ocClipboardFailure, Name + ': unexpected clipboard exception');
      Assert(Pointer(E) = Clipboard.RaisedException, Name + ': exception was replaced');
      Assert(E.Message = 'injected clipboard failure', Name + ': exception message changed');
      Raised:=True;
    end;
    on E: ERpcFailure do begin
      Assert(Outcome = ocRpcFailure, Name + ': unexpected RPC exception');
      Raised:=True;
    end;
  end;
  Assert(Raised = (Outcome in [ocClipboardFailure, ocRpcFailure]),
    Name + ': exception propagation changed');
  Assert(RpcObj.Calls = Ord(Outcome <> ocNoSelection), Name + ': RPC call count changed');
  Assert(MainForm.StatusCount = Ord(Outcome in [ocInvalid, ocNil]),
    Name + ': status count changed');
  if Outcome = ocInvalid then
    Assert(MainForm.LastStatus = 'Invalid server response.', Name + ': wrong error message')
  else
    Assert(MainForm.LastStatus = '', Name + ': unexpected error message');
  ConfirmedWrite:=(Outcome = ocClipboardFailure) and
    (WriteMode = wmFailAfterWrite) and (ReadMode = rmSuccess);
  if Outcome = ocCopy then begin
    Assert(Clipboard.Calls = 1, Name + ': wrong write count');
    Assert(Clipboard.Text = Expected, Name + ': output changed');
    Assert(MainForm.FLastClipboardLink = Expected, Name + ': marker not committed');
  end
  else begin
    if ConfirmedWrite then
      Assert(MainForm.FLastClipboardLink = Expected, Name + ': committed marker was discarded')
    else
      Assert(MainForm.FLastClipboardLink = PreviousMarker, Name + ': marker was not restored');
    Assert(Clipboard.Calls = Ord(Outcome = ocClipboardFailure), Name + ': unexpected write');
    if (Outcome = ocClipboardFailure) and (WriteMode = wmFailAfterWrite) then
      Assert(Clipboard.Text = Expected, Name + ': partial write was overwritten')
    else if (Outcome = ocClipboardFailure) and (WriteMode = wmFailAfterReplacement) then
      Assert(Clipboard.Text = ExternalLink, Name + ': external clipboard text was overwritten')
    else
      Assert(Clipboard.Text = 'original clipboard contents', Name + ': clipboard changed');
  end;
  if not MonitorDuringWrite then begin
    Assert(Clipboard.FormatCalls = Ord(Outcome = ocClipboardFailure),
      Name + ': recovery read missing or added to a normal path');
    Assert(Clipboard.ReadCalls = Ord((Outcome = ocClipboardFailure) and
      (ReadMode in [rmSuccess, rmFailRead])), Name + ': recovery read count changed');
  end;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0, Name + ': write callback queued a torrent');
  if (Outcome = ocCopy) or ConfirmedWrite then begin
    MainForm.CheckClipboardLink;
    MainForm.CheckClipboardLink;
    Assert(MainForm.FPendingClipboardTorrents.Count = 0, Name + ': own magnet queued for import');
    Assert(not MainForm.FCheckingClipboardLink, Name + ': monitor guard was not released');
  end;
  Inc(TestsRun);
  WriteLn('PASS ', Name);
end;

procedure CheckRejected(const Name, Response: string);
begin
  RunCase(Name, Response, '', ocInvalid);
end;

procedure RunTests;
const
  InvalidContainers: array[0..5] of string = ('null', 'true', '1', '1.5', '"text"', '{}');
  InvalidEntries: array[0..5] of string = ('null', 'true', '1', '1.5', '"text"', '[]');
  InvalidLinks: array[0..5] of string = ('null', 'true', '1', '1.5', '[]', '{}');
  OneResponse = '{"torrents":[{"magnetLink":"magnet:?xt=urn:btih:first"}]}';
  TwoResponse = '{"torrents":[{"magnetLink":"magnet:?xt=urn:btih:second"},' +
    '{"magnetLink":"magnet:?xt=urn:btih:first"}]}';
var
  i: Integer;
  ReadMode: TReadMode;
  OneText, TwoText: string;
begin
  OneText:='magnet:?xt=urn:btih:first' + LineEnding;
  TwoText:='magnet:?xt=urn:btih:second' + LineEnding + OneText;
  RunCase('no selection', '', '', ocNoSelection);
  RunCase('nil RPC result', '', '', ocNil);
  RunCase('RPC exception cleanup', '', '', ocRpcFailure);
  RunCase('single link', OneResponse, OneText, ocCopy);
  RunCase('multiple links preserve response order', TwoResponse, TwoText, ocCopy);
  RunCase('empty array clears clipboard', '{"torrents":[]}', '', ocCopy);
  RunCase('empty string retains line ending', '{"torrents":[{"magnetLink":""}]}',
    LineEnding, ocCopy);
  RunCase('URI syntax remains out of scope', '{"torrents":[{"magnetLink":"not a URI"}]}',
    'not a URI' + LineEnding, ocCopy);
  RunCase('unknown fields remain allowed', '{"extra":1,"torrents":[{"id":7,' +
    '"magnetLink":"magnet:?xt=urn:btih:first","extra":true}]}', OneText, ocCopy);

  CheckRejected('missing torrents', '{}');
  CheckRejected('wrong-case torrents', '{"Torrents":[]}');
  for i:=Low(InvalidContainers) to High(InvalidContainers) do
    CheckRejected('invalid container ' + IntToStr(i), '{"torrents":' + InvalidContainers[i] + '}');
  for i:=Low(InvalidEntries) to High(InvalidEntries) do
    CheckRejected('invalid element ' + IntToStr(i), '{"torrents":[' + InvalidEntries[i] + ']}');
  CheckRejected('missing magnetLink', '{"torrents":[{}]}');
  CheckRejected('wrong-case magnetLink', '{"torrents":[{"MagnetLink":"magnet:?x"}]}');
  for i:=Low(InvalidLinks) to High(InvalidLinks) do
    CheckRejected('invalid magnetLink ' + IntToStr(i),
      '{"torrents":[{"magnetLink":' + InvalidLinks[i] + '}]}');
  CheckRejected('table response', '{"torrents":[["magnetLink"],["magnet:?x"]]}');
  CheckRejected('valid prefix then invalid field',
    '{"torrents":[{"magnetLink":"magnet:?x"},{"magnetLink":null}]}');
  CheckRejected('valid prefix then invalid element',
    '{"torrents":[{"magnetLink":"magnet:?x"},null]}');

  RunCase('partial write retains committed marker', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterWrite);
  RunCase('write failure restores nonempty marker', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailBeforeWrite);
  RunCase('write failure restores empty marker', OneResponse, OneText,
    ocClipboardFailure, '', wmFailBeforeWrite);
  RunCase('failed empty-array write restores marker', '{"torrents":[]}', '',
    ocClipboardFailure, 'old marker', wmFailBeforeWrite);
  RunCase('failed empty-string write restores marker', '{"torrents":[{"magnetLink":""}]}',
    LineEnding, ocClipboardFailure, 'old marker', wmFailBeforeWrite);
  RunCase('failed multi-link write restores marker', TwoResponse, TwoText,
    ocClipboardFailure, 'old marker', wmFailBeforeWrite);
  RunCase('failed unchanged marker', OneResponse, OneText,
    ocClipboardFailure, OneText, wmFailBeforeWrite);
  RunCase('partial empty-array write keeps committed marker', '{"torrents":[]}', '',
    ocClipboardFailure, 'old marker', wmFailAfterWrite);
  RunCase('partial empty-string write keeps committed marker', '{"torrents":[{"magnetLink":""}]}',
    LineEnding, ocClipboardFailure, 'old marker', wmFailAfterWrite);
  RunCase('partial multi-link write keeps committed marker', TwoResponse, TwoText,
    ocClipboardFailure, 'old marker', wmFailAfterWrite);
  for ReadMode:=rmNoFormat to rmFailRead do begin
    RunCase('unreadable failed write ' + IntToStr(Ord(ReadMode)), OneResponse, OneText,
      ocClipboardFailure, 'old marker', wmFailBeforeWrite, ReadMode);
    RunCase('unreadable partial write ' + IntToStr(Ord(ReadMode)), OneResponse, OneText,
      ocClipboardFailure, 'old marker', wmFailAfterWrite, ReadMode);
  end;
  RunCase('monitor callback during successful write', OneResponse, OneText,
    ocCopy, 'old marker', wmSuccess, rmSuccess, True);
  RunCase('monitor callback during partial write', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterWrite, rmSuccess, True);
  RunCase('external replacement is not overwritten', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterReplacement);
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 1, 'external magnet was not queued');
  Assert(MainForm.FPendingClipboardTorrents[0] = ExternalLink, 'wrong external magnet queued');
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 1, 'external magnet was queued twice');
  RunCase('prepare retry after failed write', OneResponse, OneText,
    ocClipboardFailure, 'retry marker', wmFailBeforeWrite);
  Clipboard.WriteMode:=wmSuccess;
  MainForm.MenuItem101Click(nil);
  Assert(RpcObj.Calls = 2, 'retry must send one new request');
  Assert(Clipboard.Calls = 2, 'retry must attempt one new write');
  Assert(MainForm.StatusCount = 0, 'retry produced an unexpected error');
  Assert(MainForm.FLastClipboardLink = OneText, 'retry did not commit marker');
  Assert(Clipboard.Text = OneText, 'retry did not write clipboard');
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0, 'retry queued own magnet');
  Inc(TestsRun);
  WriteLn('PASS retry succeeds without resetting state');
end;

begin
  MainForm:=TMainForm.Create(nil);
  RpcObj:=TTestRpc.Create;
  Clipboard:=TTestClipboard.Create;
  MainForm.FPendingClipboardTorrents:=TStringList.Create;
  try
    RunTests;
    WriteLn('PASS ', TestsRun, ' copy-magnet regression cases');
  finally
    MainForm.FPendingClipboardTorrents.Free;
    Clipboard.Free;
    RpcObj.Free;
    MainForm.Free;
  end;
end.
