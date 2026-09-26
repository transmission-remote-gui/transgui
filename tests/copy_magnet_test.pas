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
    FClipboardStateUnconfirmed: Boolean;
    FClipboardWriteInProgress: Boolean;
    FClipboardWriteText: string;
    FClipboardWritePreviousLink: string;
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
    MonitorBeforeCommit: Boolean;
    MonitorExistingBeforeCommit: Boolean;
    ReplaceAfterCommit: Boolean;
    FailReadAfterMonitor: Boolean;
    FailRecoveryRead: Boolean;
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
  if MonitorExistingBeforeCommit then
    MainForm.CheckClipboardLink;
  if MonitorBeforeCommit then begin
    Text:=ExternalLink;
    MainForm.CheckClipboardLink;
  end;
  case WriteMode of
    wmSuccess, wmFailAfterWrite: Text:=Value;
    wmFailAfterReplacement: Text:=ExternalLink;
  end;
  if ReplaceAfterCommit then
    Text:=ExternalLink;
  if MonitorDuringWrite then
    MainForm.CheckClipboardLink;
  if FailReadAfterMonitor then
    ReadMode:=rmFailRead;
  if WriteMode <> wmSuccess then begin
    if FailRecoveryRead then
      ReadMode:=rmFailRead;
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
  MonitorDuringWrite: Boolean = False; const ExpectedMonitorLink: string = '';
  FailRecoveryRead: Boolean = False);
var
  Raised, ConfirmedWrite: Boolean;
begin
  MainForm.FLastClipboardLink:=PreviousMarker;
  MainForm.FLinksFromClipboard:=True;
  MainForm.FCheckingClipboardLink:=False;
  MainForm.FPendingClipboardTorrents.Clear;
  MainForm.FClipboardStateUnconfirmed:=False;
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
  Clipboard.MonitorBeforeCommit:=False;
  Clipboard.MonitorExistingBeforeCommit:=False;
  Clipboard.ReplaceAfterCommit:=False;
  Clipboard.FailReadAfterMonitor:=False;
  Clipboard.FailRecoveryRead:=FailRecoveryRead;
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
    (WriteMode = wmFailAfterWrite) and (ReadMode = rmSuccess) and
    not FailRecoveryRead;
  if Outcome = ocCopy then begin
    Assert(Clipboard.Calls = 1, Name + ': wrong write count');
    Assert(Clipboard.Text = Expected, Name + ': output changed');
    Assert(MainForm.FLastClipboardLink = Expected, Name + ': marker not committed');
  end
  else begin
    if ExpectedMonitorLink <> '' then
      Assert(MainForm.FLastClipboardLink = ExpectedMonitorLink,
        Name + ': monitor marker was overwritten')
    else if ConfirmedWrite then
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
  if ExpectedMonitorLink = '' then
    Assert(MainForm.FPendingClipboardTorrents.Count = 0, Name + ': write callback queued a torrent')
  else begin
    Assert(MainForm.FPendingClipboardTorrents.Count = 1,
      Name + ': monitor did not queue exactly one external magnet');
    Assert(MainForm.FPendingClipboardTorrents[0] = ExpectedMonitorLink,
      Name + ': monitor queued the wrong external magnet');
  end;
  if (Outcome = ocCopy) or ConfirmedWrite then begin
    MainForm.CheckClipboardLink;
    MainForm.CheckClipboardLink;
    Assert(MainForm.FPendingClipboardTorrents.Count = 0, Name + ': own magnet queued for import');
    Assert(not MainForm.FClipboardStateUnconfirmed,
      Name + ': confirmed clipboard state remained ambiguous');
    Assert(not MainForm.FCheckingClipboardLink, Name + ': monitor guard was not released');
  end;
  Inc(TestsRun);
  WriteLn('PASS ', Name);
end;

procedure CheckRejected(const Name, Response: string);
begin
  RunCase(Name, Response, '', ocInvalid);
end;

procedure CheckCallbackBeforeCommitRecovery;
const
  OneResponse = '{"torrents":[{"magnetLink":"magnet:?xt=urn:btih:first"}]}';
var
  Raised: Boolean;
  OneText: string;

  procedure PrepareCase(AWriteMode: TWriteMode; AFailRecoveryRead: Boolean);
  begin
    MainForm.FLastClipboardLink:='callback marker';
    MainForm.FClipboardStateUnconfirmed:=False;
    MainForm.FLinksFromClipboard:=True;
    MainForm.FCheckingClipboardLink:=False;
    MainForm.FPendingClipboardTorrents.Clear;
    MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
    MainForm.StatusCount:=0;
    RpcObj.Response:=OneResponse;
    RpcObj.Calls:=0;
    RpcObj.Fail:=False;
    Clipboard.Text:='original clipboard contents';
    Clipboard.Calls:=0;
    Clipboard.ReadCalls:=0;
    Clipboard.FormatCalls:=0;
    Clipboard.WriteMode:=AWriteMode;
    Clipboard.ReadMode:=rmSuccess;
    Clipboard.MonitorDuringWrite:=False;
    Clipboard.MonitorBeforeCommit:=True;
    Clipboard.FailRecoveryRead:=AFailRecoveryRead;
    Clipboard.RaisedException:=nil;
  end;

  procedure AssertExternalCallbackQueued(const Name: string);
  begin
    Assert(MainForm.FPendingClipboardTorrents.Count = 1,
      Name + ': external callback was not queued exactly once');
    Assert(MainForm.FPendingClipboardTorrents[0] = ExternalLink,
      Name + ': wrong external callback value was queued');
  end;

  procedure AssertNoOwnImport(const Name: string);
  begin
    MainForm.CheckClipboardLink;
    MainForm.CheckClipboardLink;
    Assert(MainForm.FPendingClipboardTorrents.Count = 1,
      Name + ': application magnet was queued after the callback');
    Assert(MainForm.FLastClipboardLink = OneText,
      Name + ': clipboard marker does not match the committed application text');
    Assert(not MainForm.FClipboardStateUnconfirmed,
      Name + ': clipboard state remained uncertain after synchronization');
  end;

begin
  OneText:='magnet:?xt=urn:btih:first' + LineEnding;

  PrepareCase(wmSuccess, False);
  MainForm.MenuItem101Click(nil);
  AssertExternalCallbackQueued('successful callback-before-commit');
  Assert(Clipboard.Text = OneText,
    'successful callback-before-commit did not write the application text');
  Assert(MainForm.FLastClipboardLink = OneText,
    'successful callback-before-commit marker not committed');
  AssertNoOwnImport('successful callback-before-commit');
  Inc(TestsRun);
  WriteLn('PASS successful callback before final commit');

  PrepareCase(wmFailAfterWrite, False);
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do begin
      Assert(Pointer(E) = Clipboard.RaisedException,
        'readable callback-before-commit exception was replaced');
      Assert(E.Message = 'injected clipboard failure',
        'readable callback-before-commit exception message changed');
      Raised:=True;
    end;
  end;
  Assert(Raised, 'readable callback-before-commit write did not raise');
  AssertExternalCallbackQueued('readable callback-before-commit');
  Assert(Clipboard.Text = OneText,
    'readable callback-before-commit did not leave the committed application text');
  Assert(MainForm.FLastClipboardLink = OneText,
    'readable recovery did not track the clipboard value confirmed after the callback');
  AssertNoOwnImport('readable callback-before-commit');
  Inc(TestsRun);
  WriteLn('PASS readable callback before final commit recovery');

  PrepareCase(wmFailAfterWrite, True);
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do begin
      Assert(Pointer(E) = Clipboard.RaisedException,
        'unreadable callback-before-commit exception was replaced');
      Assert(E.Message = 'injected clipboard failure',
        'unreadable callback-before-commit exception message changed');
      Raised:=True;
    end;
  end;
  Assert(Raised, 'unreadable callback-before-commit write did not raise');
  AssertExternalCallbackQueued('unreadable callback-before-commit');
  Assert(Clipboard.Text = OneText,
    'unreadable callback-before-commit did not leave the committed application text');
  Assert(MainForm.FLastClipboardLink = ExternalLink,
    'unreadable recovery overwrote the callback marker without a confirming read');
  Assert(MainForm.FClipboardStateUnconfirmed,
    'unreadable callback-before-commit recovery was marked confirmed');
  Clipboard.ReadMode:=rmSuccess;
  AssertNoOwnImport('unreadable callback-before-commit');
  Inc(TestsRun);
  WriteLn('PASS unreadable callback before final commit recovery');

  Clipboard.MonitorBeforeCommit:=False;
end;

procedure CheckExternalReplacementAfterSuccessfulCommit;
const
  OneResponse = '{"torrents":[{"magnetLink":"magnet:?xt=urn:btih:first"}]}';
  procedure PrepareCase(AFailReadAfterMonitor: Boolean);
  begin
    MainForm.FLastClipboardLink:='success replacement marker';
    MainForm.FClipboardStateUnconfirmed:=False;
    MainForm.FLinksFromClipboard:=True;
    MainForm.FCheckingClipboardLink:=False;
    MainForm.FPendingClipboardTorrents.Clear;
    MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
    MainForm.StatusCount:=0;
    RpcObj.Response:=OneResponse;
    RpcObj.Calls:=0;
    RpcObj.Fail:=False;
    Clipboard.Text:='original clipboard contents';
    Clipboard.Calls:=0;
    Clipboard.ReadCalls:=0;
    Clipboard.FormatCalls:=0;
    Clipboard.WriteMode:=wmSuccess;
    Clipboard.ReadMode:=rmSuccess;
    Clipboard.MonitorDuringWrite:=True;
    Clipboard.MonitorBeforeCommit:=False;
    Clipboard.ReplaceAfterCommit:=True;
    Clipboard.FailReadAfterMonitor:=AFailReadAfterMonitor;
    Clipboard.FailRecoveryRead:=False;
    Clipboard.RaisedException:=nil;
  end;

  procedure AssertExternalQueuedOnce(const Name: string);
  begin
    Assert(MainForm.FPendingClipboardTorrents.Count = 1,
      Name + ': external replacement was not queued exactly once');
    Assert(MainForm.FPendingClipboardTorrents[0] = ExternalLink,
      Name + ': wrong external replacement was queued');
  end;

begin
  PrepareCase(False);
  MainForm.MenuItem101Click(nil);
  Assert(Clipboard.Text = ExternalLink,
    'successful post-commit replacement did not remain on the clipboard');
  AssertExternalQueuedOnce('successful post-commit replacement');
  Assert(MainForm.FLastClipboardLink = ExternalLink,
    'successful post-commit replacement marker was overwritten');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'readable successful post-commit replacement remained uncertain');
  MainForm.CheckClipboardLink;
  MainForm.CheckClipboardLink;
  AssertExternalQueuedOnce('successful post-commit replacement after recheck');
  Inc(TestsRun);
  WriteLn('PASS external replacement after successful commit');

  PrepareCase(True);
  MainForm.MenuItem101Click(nil);
  Assert(Clipboard.Text = ExternalLink,
    'unreadable successful post-commit replacement did not remain on the clipboard');
  AssertExternalQueuedOnce('unreadable successful post-commit replacement');
  Assert(MainForm.FLastClipboardLink = ExternalLink,
    'unreadable successful post-commit replacement lost the callback marker');
  Assert(MainForm.FClipboardStateUnconfirmed,
    'unreadable successful post-commit replacement was marked confirmed');
  Clipboard.ReadMode:=rmSuccess;
  MainForm.CheckClipboardLink;
  MainForm.CheckClipboardLink;
  AssertExternalQueuedOnce('unreadable successful post-commit replacement after recovery');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'successful post-commit replacement recovery did not clear uncertainty');
  Inc(TestsRun);
  WriteLn('PASS unreadable external replacement after successful commit');

  Clipboard.MonitorDuringWrite:=False;
  Clipboard.ReplaceAfterCommit:=False;
  Clipboard.FailReadAfterMonitor:=False;
end;

procedure CheckWriteInProgressSuppression;
const
  AppLink = 'magnet:?xt=urn:btih:first';
  PreviousLink = 'magnet:?xt=urn:btih:previous';
  OneResponse = '{"torrents":[{"magnetLink":"' + AppLink + '"}]}';
var
  Raised: Boolean;
  AppText: string;

  procedure PrepareCase(AWriteMode: TWriteMode);
  begin
    MainForm.FLastClipboardLink:=PreviousLink;
    MainForm.FClipboardStateUnconfirmed:=False;
    MainForm.FClipboardWriteInProgress:=False;
    MainForm.FClipboardWriteText:='';
    MainForm.FClipboardWritePreviousLink:='';
    MainForm.FLinksFromClipboard:=True;
    MainForm.FCheckingClipboardLink:=False;
    MainForm.FPendingClipboardTorrents.Clear;
    MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
    MainForm.StatusCount:=0;
    RpcObj.Response:=OneResponse;
    RpcObj.Calls:=0;
    RpcObj.Fail:=False;
    Clipboard.Text:=PreviousLink;
    Clipboard.Calls:=0;
    Clipboard.ReadCalls:=0;
    Clipboard.FormatCalls:=0;
    Clipboard.WriteMode:=AWriteMode;
    Clipboard.ReadMode:=rmSuccess;
    Clipboard.MonitorDuringWrite:=False;
    Clipboard.MonitorBeforeCommit:=False;
    Clipboard.MonitorExistingBeforeCommit:=False;
    Clipboard.ReplaceAfterCommit:=False;
    Clipboard.FailReadAfterMonitor:=False;
    Clipboard.FailRecoveryRead:=False;
    Clipboard.RaisedException:=nil;
  end;

  procedure AssertWriteStateCleared(const Name: string);
  begin
    Assert(not MainForm.FClipboardWriteInProgress,
      Name + ': clipboard write-in-progress flag was not cleared');
    Assert(MainForm.FClipboardWriteText = '',
      Name + ': clipboard write payload was not cleared');
    Assert(MainForm.FClipboardWritePreviousLink = '',
      Name + ': previous clipboard marker was not cleared');
  end;

  procedure AssertOnlyExternalQueued(const Name: string);
  begin
    Assert(MainForm.FPendingClipboardTorrents.Count = 1,
      Name + ': expected exactly one external magnet');
    Assert(MainForm.FPendingClipboardTorrents[0] = ExternalLink,
      Name + ': application payload or wrong external value was queued');
  end;

begin
  AppText:=AppLink + LineEnding;

  PrepareCase(wmSuccess);
  Clipboard.MonitorExistingBeforeCommit:=True;
  MainForm.MenuItem101Click(nil);
  Assert(MainForm.FPendingClipboardTorrents.Count = 0,
    'pre-commit previous marker was queued again');
  Assert(MainForm.FLastClipboardLink = AppText,
    'successful pre-commit previous-marker case lost the application marker');
  AssertWriteStateCleared('pre-commit previous marker');
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0,
    'application payload queued after pre-commit previous-marker callback');
  Inc(TestsRun);
  WriteLn('PASS previous marker suppressed while clipboard write is active');

  PrepareCase(wmSuccess);
  Clipboard.MonitorBeforeCommit:=True;
  Clipboard.MonitorDuringWrite:=True;
  MainForm.MenuItem101Click(nil);
  AssertOnlyExternalQueued('successful dual callback');
  Assert(MainForm.FLastClipboardLink = AppText,
    'successful dual callback did not finish on the application marker');
  AssertWriteStateCleared('successful dual callback');
  MainForm.CheckClipboardLink;
  MainForm.CheckClipboardLink;
  AssertOnlyExternalQueued('successful dual callback after recheck');
  Inc(TestsRun);
  WriteLn('PASS own payload suppressed during successful dual callbacks');

  PrepareCase(wmFailAfterWrite);
  Clipboard.MonitorBeforeCommit:=True;
  Clipboard.MonitorDuringWrite:=True;
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do begin
      Assert(Pointer(E) = Clipboard.RaisedException,
        'dual-callback failure replaced the original exception');
      Raised:=True;
    end;
  end;
  Assert(Raised, 'dual-callback failed write did not raise');
  AssertOnlyExternalQueued('failed dual callback');
  Assert(MainForm.FLastClipboardLink = AppText,
    'failed dual callback did not recover the committed application marker');
  AssertWriteStateCleared('failed dual callback');
  MainForm.CheckClipboardLink;
  MainForm.CheckClipboardLink;
  AssertOnlyExternalQueued('failed dual callback after recheck');
  Inc(TestsRun);
  WriteLn('PASS own payload suppressed during failed dual callbacks');

  Clipboard.MonitorExistingBeforeCommit:=False;
  Clipboard.MonitorBeforeCommit:=False;
  Clipboard.MonitorDuringWrite:=False;
end;

procedure CheckRepeatedAmbiguousWrites;
const
  FirstLink = 'magnet:?xt=urn:btih:resident';
var
  i: Integer;
  CandidateText, FirstText: string;
  Raised: Boolean;
begin
  FirstText:=FirstLink + LineEnding;
  MainForm.FLastClipboardLink:='repeat marker';
  MainForm.FClipboardStateUnconfirmed:=False;
  MainForm.FLinksFromClipboard:=True;
  MainForm.FCheckingClipboardLink:=False;
  MainForm.FPendingClipboardTorrents.Clear;
  MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
  MainForm.StatusCount:=0;
  RpcObj.Fail:=False;
  Clipboard.Text:='original clipboard contents';
  Clipboard.Calls:=0;
  Clipboard.ReadCalls:=0;
  Clipboard.FormatCalls:=0;
  Clipboard.ReadMode:=rmNoFormat;
  Clipboard.MonitorDuringWrite:=False;
  Clipboard.FailRecoveryRead:=False;

  for i:=0 to 31 do begin
    if i = 0 then
      CandidateText:=FirstLink
    else
      CandidateText:='magnet:?xt=urn:btih:later' + IntToStr(i);
    RpcObj.Response:='{"torrents":[{"magnetLink":"' + CandidateText + '"}]}';
    if i = 0 then
      Clipboard.WriteMode:=wmFailAfterWrite
    else
      Clipboard.WriteMode:=wmFailBeforeWrite;
    Raised:=False;
    try
      MainForm.MenuItem101Click(nil);
    except
      on E: EClipboardWrite do
        Raised:=True;
    end;
    Assert(Raised, 'repeated ambiguous write did not raise');
    Assert(MainForm.FLastClipboardLink = 'repeat marker',
      'repeated ambiguous write changed the confirmed marker');
    Assert(MainForm.FClipboardStateUnconfirmed,
      'repeated ambiguous write lost the uncertain state');
  end;

  Assert(Clipboard.Text = FirstText,
    'later fail-before-write attempts changed the resident clipboard payload');
  Clipboard.ReadMode:=rmSuccess;
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0,
    'resident application clipboard payload was queued after repeated failures');
  Assert(MainForm.FLastClipboardLink = FirstText,
    'resident application clipboard payload did not become the confirmed marker');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'successful synchronization did not clear the uncertain state');
  Inc(TestsRun);
  WriteLn('PASS repeated ambiguous clipboard writes');
end;

procedure CheckRecoveryReadResolvesEarlierAmbiguity;
const
  FirstLink = 'magnet:?xt=urn:btih:earlier';
  SecondLink = 'magnet:?xt=urn:btih:later';
var
  Raised: Boolean;
  FirstText: string;
begin
  FirstText:=FirstLink + LineEnding;
  MainForm.FLastClipboardLink:='recovery marker';
  MainForm.FClipboardStateUnconfirmed:=False;
  MainForm.FLinksFromClipboard:=True;
  MainForm.FCheckingClipboardLink:=False;
  MainForm.FPendingClipboardTorrents.Clear;
  MainForm.SelectedTorrents:=VarArrayOf([7, 42]);
  MainForm.StatusCount:=0;
  RpcObj.Fail:=False;
  Clipboard.Text:='original clipboard contents';
  Clipboard.Calls:=0;
  Clipboard.ReadCalls:=0;
  Clipboard.FormatCalls:=0;
  Clipboard.MonitorDuringWrite:=False;
  Clipboard.FailRecoveryRead:=False;

  RpcObj.Response:='{"torrents":[{"magnetLink":"' + FirstLink + '"}]}';
  Clipboard.WriteMode:=wmFailAfterWrite;
  Clipboard.ReadMode:=rmNoFormat;
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do
      Raised:=True;
  end;
  Assert(Raised, 'earlier ambiguous write did not raise');
  Assert(MainForm.FClipboardStateUnconfirmed,
    'earlier ambiguous write did not mark the clipboard state uncertain');
  Assert(Clipboard.Text = FirstText, 'earlier committed payload was not retained');

  RpcObj.Response:='{"torrents":[{"magnetLink":"' + SecondLink + '"}]}';
  Clipboard.WriteMode:=wmFailBeforeWrite;
  Clipboard.ReadMode:=rmSuccess;
  Raised:=False;
  try
    MainForm.MenuItem101Click(nil);
  except
    on E: EClipboardWrite do
      Raised:=True;
  end;
  Assert(Raised, 'later recovery write did not raise');
  Assert(MainForm.FLastClipboardLink = FirstText,
    'successful recovery read did not synchronize the earlier clipboard state');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'successful recovery read did not clear the uncertain state');
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0,
    'earlier application payload was queued after recovery synchronization');
  Inc(TestsRun);
  WriteLn('PASS recovery read resolves earlier ambiguous write');
end;

procedure CheckExternalChangeWhileUnconfirmed;
begin
  MainForm.FLastClipboardLink:='external-sync marker';
  MainForm.FClipboardStateUnconfirmed:=True;
  MainForm.FLinksFromClipboard:=True;
  MainForm.FCheckingClipboardLink:=False;
  MainForm.FPendingClipboardTorrents.Clear;
  Clipboard.Text:=ExternalLink;
  Clipboard.ReadMode:=rmSuccess;
  Clipboard.FailRecoveryRead:=False;

  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 0,
    'uncertain clipboard recovery imported an external value');
  Assert(MainForm.FLastClipboardLink = ExternalLink,
    'uncertain clipboard recovery did not synchronize the observed value');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'uncertain clipboard recovery did not clear the state flag');
  Inc(TestsRun);
  WriteLn('PASS external change while clipboard state is uncertain');
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
    Clipboard.ReadMode:=rmSuccess;
    MainForm.CheckClipboardLink;
    Assert(MainForm.FPendingClipboardTorrents.Count = 0,
      'failed write queued clipboard contents after recovery');
    Assert(not MainForm.FClipboardStateUnconfirmed,
      'failed write uncertainty was not cleared after recovery');

    RunCase('unreadable partial write ' + IntToStr(Ord(ReadMode)), OneResponse, OneText,
      ocClipboardFailure, 'old marker', wmFailAfterWrite, ReadMode);
    Clipboard.ReadMode:=rmSuccess;
    MainForm.CheckClipboardLink;
    Assert(MainForm.FPendingClipboardTorrents.Count = 0,
      'unreadable partial write queued the application''s own magnet');
    Assert(MainForm.FLastClipboardLink = OneText,
      'unreadable partial write did not commit the marker after recovery');
    Assert(not MainForm.FClipboardStateUnconfirmed,
      'partial write uncertainty was not cleared after recovery');
  end;
  RunCase('monitor callback during successful write', OneResponse, OneText,
    ocCopy, 'old marker', wmSuccess, rmSuccess, True);
  RunCase('monitor callback during partial write', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterWrite, rmSuccess, True);
  RunCase('external replacement monitor survives recovery', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterReplacement, rmSuccess, True, ExternalLink);
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 1,
    'external replacement was queued twice after recovery');

  RunCase('external replacement monitor survives unreadable recovery', OneResponse, OneText,
    ocClipboardFailure, 'old marker', wmFailAfterReplacement, rmSuccess, True, ExternalLink, True);
  Assert(MainForm.FClipboardStateUnconfirmed,
    'unreadable external replacement recovery was marked confirmed');
  Clipboard.ReadMode:=rmSuccess;
  MainForm.CheckClipboardLink;
  Assert(MainForm.FPendingClipboardTorrents.Count = 1,
    'external replacement was queued twice after unreadable recovery');
  Assert(not MainForm.FClipboardStateUnconfirmed,
    'successful external replacement synchronization did not clear uncertainty');

  CheckCallbackBeforeCommitRecovery;
  CheckExternalReplacementAfterSuccessfulCommit;
  CheckWriteInProgressSuppression;
  CheckRepeatedAmbiguousWrites;
  CheckRecoveryReadResolvesEarlierAmbiguity;
  CheckExternalChangeWhileUnconfirmed;

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
