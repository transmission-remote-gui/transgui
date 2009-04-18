{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2009 by Yury Sidorov.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*************************************************************************************}

unit rpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, httpsend, syncobjs, fpjson, jsonparser;

type
  TAdvInfoType = (aiNone, aiGeneral, aiFiles, aiPeers);
  TRefreshType = (rtNone, rtAll, rtDetails);

  TRpc = class;

  { TRpcThread }

  TRpcThread = class(TThread)
  private
    ResultData: TJSONData;
    FRpc: TRpc;
    FRPCVersion: integer;

    function GetAdvInfo: TAdvInfoType;
    function GetCurTorrentId: cardinal;
    function GetRefreshInterval: TDateTime;
    function GetStatus: string;
    procedure SetStatus(const AValue: string);

    function GetTorrents: boolean;
    procedure GetPeers(TorrentId: integer);
    procedure GetFiles(TorrentId: integer);
    procedure GetInfo(TorrentId: integer);
    procedure GetStatusInfo;

    procedure DoFillTorrentsList;
    procedure DoFillPeersList;
    procedure DoFillFilesList;
    procedure DoFillInfo;
    procedure NotifyCheckStatus;
    procedure CheckStatusHandler(Data: PtrInt);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Status: string read GetStatus write SetStatus;
    property RefreshInterval: TDateTime read GetRefreshInterval;
    property CurTorrentId: cardinal read GetCurTorrentId;
    property AdvInfo: TAdvInfoType read GetAdvInfo;
  end;

  TRpc = class
  private
    FLock: TCriticalSection;
    FStatus: string;
    FInfoStatus: string;
    FConnected: boolean;
    FTorrentFields: string;

    function GetConnected: boolean;
    function GetInfoStatus: string;
    function GetRPCVersion: integer;
    function GetStatus: string;
    function GetTorrentFields: string;
    procedure SetInfoStatus(const AValue: string);
    procedure SetStatus(const AValue: string);
    procedure SetTorrentFields(const AValue: string);
  public
    Http: THTTPSend;
    HttpLock: TCriticalSection;
    RpcThread: TRpcThread;
    Url: string;
    RefreshInterval: TDateTime;
    CurTorrentId: cardinal;
    AdvInfo: TAdvInfoType;
    RefreshNow: TRefreshType;
    RequestFullInfo: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    procedure Connect;
    procedure Disconnect;

    function SendRequest(req: TJSONObject; ReturnArguments: boolean = True): TJSONObject;
    function RequestInfo(TorrentId: integer; const Fields: array of const; const ExtraFields: array of string): TJSONObject;
    function RequestInfo(TorrentId: integer; const Fields: array of const): TJSONObject;

    property Status: string read GetStatus write SetStatus;
    property InfoStatus: string read GetInfoStatus write SetInfoStatus;
    property Connected: boolean read GetConnected;
    property TorrentFields: string read GetTorrentFields write SetTorrentFields;
    property RPCVersion: integer read GetRPCVersion;
  end;

implementation

uses Main;

{ TRpcThread }

procedure TRpcThread.Execute;
var
  t: TDateTime;
  r: TRefreshType;
begin
  try
    GetStatusInfo;
    if Status <> '' then
      Terminate
    else
      FRpc.FConnected:=True;
    NotifyCheckStatus;

    t:=Now - 1;
    while not Terminated do begin
      if (Now - t >= RefreshInterval) or (FRpc.RefreshNow <> rtNone) then begin
        r:=FRpc.RefreshNow;
        FRpc.RefreshNow:=rtNone;
        if (r = rtDetails) or GetTorrents then
          if not Terminated and (CurTorrentId <> 0) then begin
            case AdvInfo of
              aiGeneral:
                GetInfo(CurTorrentId);
              aiPeers:
                GetPeers(CurTorrentId);
              aiFiles:
                GetFiles(CurTorrentId);
            end;
            if FRpc.RefreshNow = rtDetails then
              FRpc.RefreshNow:=rtNone;
          end;

        NotifyCheckStatus;
        t:=Now;
      end;
      Sleep(50);
    end;
  except
    Status:=Exception(ExceptObject).Message;
    NotifyCheckStatus;
  end;
  FRpc.RpcThread:=nil;
  FRpc.FConnected:=False;
  Sleep(20);
end;

constructor TRpcThread.Create;
begin
  inherited Create(True);
end;

destructor TRpcThread.Destroy;
begin
  inherited Destroy;
end;

procedure TRpcThread.SetStatus(const AValue: string);
begin
  FRpc.Status:=AValue;
end;

procedure TRpcThread.DoFillTorrentsList;
begin
  MainForm.FillTorrentsList(ResultData as TJSONArray);
end;

procedure TRpcThread.DoFillPeersList;
begin
  MainForm.FillPeersList(ResultData as TJSONArray);
end;

procedure TRpcThread.DoFillFilesList;
var
  t: TJSONObject;
begin
  if ResultData = nil then begin
    MainForm.ClearDetailsInfo;
    exit;
  end;
  t:=ResultData as TJSONObject;
  MainForm.FillFilesList(t.Arrays['files'], t.Arrays['priorities'], t.Arrays['wanted']);
end;

procedure TRpcThread.DoFillInfo;
begin
  MainForm.FillGeneralInfo(ResultData as TJSONObject);
end;

procedure TRpcThread.NotifyCheckStatus;
begin
  Application.QueueAsyncCall(@CheckStatusHandler, 0);
end;

procedure TRpcThread.CheckStatusHandler(Data: PtrInt);
begin
  if csDestroying in MainForm.ComponentState then exit;
  MainForm.CheckStatus;
end;

procedure TRpcThread.GetStatusInfo;
var
  req, args: TJSONObject;
  s: string;
begin
  req:=TJSONObject.Create;
  try
    req.Add('method', 'session-get');
    args:=FRpc.SendRequest(req);
    if args <> nil then
    try
      if args.IndexOfName('rpc-version') >= 0 then
        FRPCVersion := args.Integers['rpc-version']
      else
        FRPCVersion := -1;
      if args.IndexOfName('version') >= 0 then
        s:=' ' + args.Strings['version']
      else
        s:='';
      FRpc.InfoStatus:=Format('Transmission%s at %s:%s', [s, FRpc.Http.TargetHost, FRpc.Http.TargetPort]);
    finally
      args.Free;
    end;
  finally
    req.Free;
  end;
end;

function TRpcThread.GetTorrents: boolean;
var
  args: TJSONObject;
  ExtraFields: array of string;
  sl: TStringList;
  i: integer;
begin
  Result:=False;
  sl:=TStringList.Create;
  try
    FRpc.Lock;
    try
      sl.CommaText:=FRpc.FTorrentFields;
    finally
      FRpc.Unlock;
    end;
    i:=sl.IndexOf('trackers');
    if FRpc.RequestFullInfo then begin
      if i < 0 then
        sl.Add('trackers');
    end
    else
      if i >= 0 then
        sl.Delete(i);
    SetLength(ExtraFields, sl.Count);
    for i:=0 to sl.Count - 1 do
      ExtraFields[i]:=sl[i];
  finally
    sl.Free;
  end;

  args:=FRpc.RequestInfo(0, ['id', 'name', 'status', 'errorString', 'announceResponse', 'recheckProgress',
                             'sizeWhenDone', 'leftUntilDone', 'rateDownload', 'rateUpload'], ExtraFields);
  try
    if (args <> nil) and not Terminated then begin
      FRpc.RequestFullInfo:=False;
      ResultData:=args.Arrays['torrents'];
      Synchronize(@DoFillTorrentsList);
      Result:=True;
    end;
  finally
    args.Free;
  end;
end;

procedure TRpcThread.GetPeers(TorrentId: integer);
var
  args: TJSONObject;
  t: TJSONArray;
begin
  args:=FRpc.RequestInfo(TorrentId, ['peers']);
  try
    if args <> nil then begin
      t:=args.Arrays['torrents'];
      if t.Count > 0 then
        ResultData:=t.Objects[0].Arrays['peers']
      else
        ResultData:=nil;
      if not Terminated then
        Synchronize(@DoFillPeersList);
    end;
  finally
    args.Free;
  end;
end;

procedure TRpcThread.GetFiles(TorrentId: integer);
var
  args: TJSONObject;
  t: TJSONArray;
begin
  args:=FRpc.RequestInfo(TorrentId, ['files','priorities','wanted']);
  try
    if args <> nil then begin
      t:=args.Arrays['torrents'];
      if t.Count > 0 then
        ResultData:=t.Objects[0]
      else
        ResultData:=nil;
      if not Terminated then
        Synchronize(@DoFillFilesList);
    end;
  finally
    args.Free;
  end;
end;

procedure TRpcThread.GetInfo(TorrentId: integer);
var
  args: TJSONObject;
  t: TJSONArray;
begin
  args:=FRpc.RequestInfo(TorrentId, ['totalSize', 'sizeWhenDone', 'leftUntilDone', 'pieceCount', 'pieceSize', 'haveValid',
                                     'hashString', 'comment', 'downloadedEver', 'uploadedEver', 'corruptEver', 'errorString',
                                     'announceResponse', 'downloadLimit', 'downloadLimitMode', 'uploadLimit', 'uploadLimitMode',
                                     'maxConnectedPeers', 'nextAnnounceTime', 'dateCreated', 'creator', 'eta', 'peersSendingToUs',
                                     'seeders','peersGettingFromUs','leechers','peersKnown', 'uploadRatio', 'addedDate', 'doneDate',
                                     'activityDate', 'downloadLimited', 'uploadLimited']);
  try
    if args <> nil then begin
      t:=args.Arrays['torrents'];
      if t.Count > 0 then
        ResultData:=t.Objects[0]
      else
        ResultData:=nil;
      if not Terminated then
        Synchronize(@DoFillInfo);
    end;
  finally
    args.Free;
  end;
end;

function TRpcThread.GetAdvInfo: TAdvInfoType;
begin
  FRpc.Lock;
  try
    Result:=FRpc.AdvInfo;
  finally
    FRpc.Unlock;
  end;
end;

function TRpcThread.GetCurTorrentId: cardinal;
begin
  FRpc.Lock;
  try
    Result:=FRpc.CurTorrentId;
  finally
    FRpc.Unlock;
  end;
end;

function TRpcThread.GetRefreshInterval: TDateTime;
begin
  FRpc.Lock;
  try
    Result:=FRpc.RefreshInterval;
  finally
    FRpc.Unlock;
  end;
end;

function TRpcThread.GetStatus: string;
begin
  Result:=FRpc.Status;
end;

{ TRpc }

constructor TRpc.Create;
begin
  inherited;
  FLock:=TCriticalSection.Create;
  HttpLock:=TCriticalSection.Create;
  Http:=THTTPSend.Create;
  Http.Protocol:='1.1';
  RefreshNow:=rtNone;
end;

destructor TRpc.Destroy;
begin
  Http.Free;
  HttpLock.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRpc.SendRequest(req: TJSONObject; ReturnArguments: boolean): TJSONObject;
var
  obj: TJSONData;
  res: TJSONObject;
  jp: TJSONParser;
  s: string;
  i: integer;
  locked: boolean;
begin
  Status:='';
  Result:=nil;
  for i:=1 to 1 do begin
    HttpLock.Enter;
    locked:=True;
    try
      Http.Document.Clear;
      s:=req.AsJSON;
      Http.Document.Write(PChar(s)^, Length(s));
      Http.Headers.Clear;
      if not Http.HTTPMethod('POST', Url) then begin
        Status:=Http.Sock.LastErrorDesc;
        break;
      end
      else begin
        if Http.ResultCode <> 200 then begin
          SetString(s, Http.Document.Memory, Http.Document.Size);
          s:=StringReplace(s, '<p>', LineEnding, [rfReplaceAll, rfIgnoreCase]);
          s:=StringReplace(s, '</p>', '', [rfReplaceAll, rfIgnoreCase]);
          s:=StringReplace(s, '<h1>', '', [rfReplaceAll, rfIgnoreCase]);
          s:=StringReplace(s, '</h1>', '', [rfReplaceAll, rfIgnoreCase]);
          if s <> '' then
            Status:=s
          else
            Status:=Http.ResultString;
          continue;
        end;
//        Http.Document.SaveToFile('c:\out.txt');
        Http.Document.Position:=0;
        jp:=TJSONParser.Create(Http.Document);
        HttpLock.Leave;
        locked:=False;
        try
          try
            obj:=jp.Parse;
          except
            on E: Exception do
              begin
                Status:=e.Message;
                continue;
              end;
          end;
          try
            if obj is TJSONObject then begin
              res:=obj as TJSONObject;
              s:=res.Strings['result'];
              if AnsiCompareText(s, 'success') <> 0 then
                Status:=s
              else begin
                if ReturnArguments then begin
                  res:=res.Objects['arguments'];
                  if res = nil then
                    Status:='Arguments object not found.'
                  else begin
                    FreeAndNil(jp);
                    jp:=TJSONParser.Create(res.AsJSON);
                    Result:=TJSONObject(jp.Parse);
                    FreeAndNil(obj);
                  end;
                end
                else
                  Result:=res;
                if Result <> nil then
                  obj:=nil;
              end;
              break;
            end
            else begin
              Status:='Invalid server response.';
              continue;
            end;
          finally
            obj.Free;
          end;
        finally
          jp.Free;
        end;
      end;
    finally
      if locked then
        HttpLock.Leave;
    end;
  end;
end;

function TRpc.RequestInfo(TorrentId: integer; const Fields: array of const; const ExtraFields: array of string): TJSONObject;
var
  req, args: TJSONObject;
  _fields: TJSONArray;
  i: integer;
begin
  Result:=nil;
  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-get');
    args:=TJSONObject.Create;
    if TorrentId <> 0 then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    _fields:=TJSONArray.Create(Fields);
    for i:=Low(ExtraFields) to High(ExtraFields) do
      _fields.Add(ExtraFields[i]);
    args.Add('fields', _fields);
    req.Add('arguments', args);
    Result:=SendRequest(req);
  finally
    req.Free;
  end;
end;

function TRpc.RequestInfo(TorrentId: integer; const Fields: array of const): TJSONObject;
begin
  Result:=RequestInfo(TorrentId, Fields, []);
end;


function TRpc.GetStatus: string;
begin
  Lock;
  try
    Result:=FStatus;
    UniqueString(Result);
  finally
    Unlock;
  end;
end;

function TRpc.GetTorrentFields: string;
begin
  Lock;
  try
    Result:=FTorrentFields;
    UniqueString(Result);
  finally
    Unlock;
  end;
end;

procedure TRpc.SetInfoStatus(const AValue: string);
begin
  Lock;
  try
    FInfoStatus:=AValue;
    UniqueString(FStatus);
  finally
    Unlock;
  end;
end;

function TRpc.GetConnected: boolean;
begin
  Result:=Assigned(RpcThread) and FConnected;
end;

function TRpc.GetInfoStatus: string;
begin
  Lock;
  try
    Result:=FInfoStatus;
    UniqueString(Result);
  finally
    Unlock;
  end;
end;

function TRpc.GetRPCVersion: integer;
begin
  if Assigned(RpcThread) then
    Result:=RpcThread.FRPCVersion
  else
    Result:=-1;
end;

procedure TRpc.SetStatus(const AValue: string);
begin
  Lock;
  try
    FStatus:=AValue;
    UniqueString(FStatus);
  finally
    Unlock;
  end;
end;

procedure TRpc.SetTorrentFields(const AValue: string);
begin
  Lock;
  try
    FTorrentFields:=AValue;
    UniqueString(FTorrentFields);
  finally
    Unlock;
  end;
end;

procedure TRpc.Lock;
begin
  FLock.Enter;
end;

procedure TRpc.Unlock;
begin
  FLock.Leave;
end;

procedure TRpc.Connect;
begin
  CurTorrentId:=0;
  RequestFullInfo:=True;
  RpcThread:=TRpcThread.Create;
  with RpcThread do begin
    FreeOnTerminate:=True;
    FRpc:=Self;
    Resume;
  end;
end;

procedure TRpc.Disconnect;
begin
  if Assigned(RpcThread) then begin
    RpcThread.Terminate;
    while FConnected do begin
      Application.ProcessMessages;
      Sleep(20);
    end;
  end;
end;

end.

