program CopyMagnetTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Variants, fpjson, jsonparser;

type
  EClipboardWrite = class(Exception);
  ERpcFailure = class(Exception);
  TWriteMode = (wmSuccess, wmFailBeforeWrite, wmFailAfterWrite);
  TOutcome = (ocCopy, ocInvalid, ocNil, ocNoSelection, ocClipboardFailure, ocRpcFailure);

  TMainForm = class
    FLastClipboardLink: string;
    SelectedTorrents: Variant;
    StatusCount: Integer;
    LastStatus: string;
    function GetSelectedTorrents: Variant;
    procedure CheckStatus(Fatal: Boolean; const AStatus: string = '');
    procedure MenuItem101Click(Sender: TObject);
  end;

  TTestRpc = class
    Response: string;
    Calls: Integer;
    Fail: Boolean;
    function SendRequest(req: TJSONObject): TJSONObject;
  end;

  TTestClipboard = class
  private
    procedure SetAsText(const Value: string);
  public
    Text: string;
    Calls: Integer;
    WriteMode: TWriteMode;
    RaisedException: Pointer;
    property AsText: string read Text write SetAsText;
  end;

var
  MainForm: TMainForm;
  RpcObj: TTestRpc;
  Clipboard: TTestClipboard;
  TestsRun: Integer;

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

procedure TTestClipboard.SetAsText(const Value: string);
var
  Error: EClipboardWrite;
begin
  Inc(Calls);
  // A clipboard callback must already see the marker for our own new text.
  // This assertion detects the tempting but incorrect write-before-marker fix.
  Assert(MainForm.FLastClipboardLink = Value, 'marker was not set before the write');
  if WriteMode <> wmFailBeforeWrite then
    Text:=Value;
  if WriteMode <> wmSuccess then begin
    Error:=EClipboardWrite.Create('injected clipboard failure');
    RaisedException:=Pointer(Error);
    raise Error;
  end;
end;

// The runner extracts this implementation verbatim from the selected main.pas.
// Only external collaborators above are replaced, not the production handler.
{$I copy_magnet_handler.inc}

procedure RunCase(const Name, Response, Expected: string; Outcome: TOutcome;
  const PreviousMarker: string = 'previously observed text';
  WriteMode: TWriteMode = wmSuccess);
var
  Raised: Boolean;
begin
  MainForm.FLastClipboardLink:=PreviousMarker;
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
  Clipboard.WriteMode:=WriteMode;
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
  if Outcome = ocCopy then begin
    Assert(Clipboard.Calls = 1, Name + ': wrong write count');
    Assert(Clipboard.Text = Expected, Name + ': output changed');
    Assert(MainForm.FLastClipboardLink = Expected, Name + ': marker not committed');
  end
  else begin
    Assert(MainForm.FLastClipboardLink = PreviousMarker, Name + ': marker was not restored');
    Assert(Clipboard.Calls = Ord(Outcome = ocClipboardFailure), Name + ': unexpected write');
    if (Outcome = ocClipboardFailure) and (WriteMode = wmFailAfterWrite) then
      // Never overwrite OS clipboard contents in an attempt to undo a partial write.
      Assert(Clipboard.Text = Expected, Name + ': partial write was overwritten')
    else
      Assert(Clipboard.Text = 'original clipboard contents', Name + ': clipboard changed');
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
  RunCase('partial write restores only the internal marker', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterWrite);
  RunCase('prepare retry after failed write', OneResponse, OneText,
    ocClipboardFailure, 'retry marker', wmFailBeforeWrite);
  // Retry on the same form and clipboard without resetting their state.
  Clipboard.WriteMode:=wmSuccess;
  MainForm.MenuItem101Click(nil);
  Assert(RpcObj.Calls = 2, 'retry must send one new request');
  Assert(Clipboard.Calls = 2, 'retry must attempt one new write');
  Assert(MainForm.StatusCount = 0, 'retry produced an unexpected error');
  Assert(MainForm.FLastClipboardLink = OneText, 'retry did not commit marker');
  Assert(Clipboard.Text = OneText, 'retry did not write clipboard');
  Inc(TestsRun);
  WriteLn('PASS retry succeeds without resetting state');
end;

begin
  MainForm:=TMainForm.Create;
  RpcObj:=TTestRpc.Create;
  Clipboard:=TTestClipboard.Create;
  try
    RunTests;
    WriteLn('PASS ', TestsRun, ' copy-magnet regression cases');
  finally
    Clipboard.Free;
    RpcObj.Free;
    MainForm.Free;
  end;
end.
