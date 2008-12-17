{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008 by Yury Sidorov.

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

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, ActnList,
  httpsend, IniFiles, StdCtrls, fpjson, jsonparser, ExtCtrls, rpc, syncobjs, variants, varlist;

const
  AppName = 'Transmission Remote GUI';

type

  { TMainForm }

  TMainForm = class(TForm)
    acConnect: TAction;
    acAddTorrent: TAction;
    acStopTorrent: TAction;
    acRemoveTorrent: TAction;
    acStartTorrent: TAction;
    acSetHighPriority: TAction;
    acSetNormalPriority: TAction;
    acSetLowPriority: TAction;
    acSetNotDownload: TAction;
    acConnOptions: TAction;
    acOptions: TAction;
    acVerifyTorrent: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    ImageList16: TImageList;
    txError: TLabel;
    txErrorLabel: TLabel;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    miOptions: TMenuItem;
    DummyTimer: TTimer;
    MainToolBar: TToolBar;
    panTransfer: TPanel;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    txComment: TLabel;
    txCommentLabel: TLabel;
    txHash: TLabel;
    txHashLabel: TLabel;
    panGeneralInfo: TPanel;
    lvFiles: TListView;
    lvPeers: TListView;
    lvTorrents: TListView;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miConnect: TMenuItem;
    miExit: TMenuItem;
    miTorrent: TMenuItem;
    OpenTorrentDlg: TOpenDialog;
    PageInfo: TPageControl;
    pmTorrents: TPopupMenu;
    pmFiles: TPopupMenu;
    ScrollBox1: TScrollBox;
    txPieces: TLabel;
    txPiecesLabel: TLabel;
    txTotalSize: TLabel;
    txTotalSizeLabel: TLabel;
    VSplitter: TSplitter;
    StatusBar: TStatusBar;
    tabPeers: TTabSheet;
    tabGeneral: TTabSheet;
    DetailsTimer: TTimer;
    tabFiles: TTabSheet;
    procedure acAddTorrentExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acConnOptionsExecute(Sender: TObject);
    procedure acDisconnectExecute(Sender: TObject);
    procedure acRemoveTorrentExecute(Sender: TObject);
    procedure acSetHighPriorityExecute(Sender: TObject);
    procedure acSetLowPriorityExecute(Sender: TObject);
    procedure acSetNormalPriorityExecute(Sender: TObject);
    procedure acSetNotDownloadExecute(Sender: TObject);
    procedure acStartTorrentExecute(Sender: TObject);
    procedure acStopTorrentExecute(Sender: TObject);
    procedure acVerifyTorrentExecute(Sender: TObject);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure DummyTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvTorrentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvTorrentsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvTorrentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miExitClick(Sender: TObject);
    procedure PageInfoChange(Sender: TObject);
    procedure DetailsTimerTimer(Sender: TObject);
    procedure pmFilesPopup(Sender: TObject);
    procedure pmTorrentsPopup(Sender: TObject);
  private
    FIni: TIniFile;
    FStarted: boolean;
    FTorrents: TVarList;
    FTorrentsSortColumn: integer;
    FTorrentsSortDesc: boolean;

    procedure DoConnect;
    procedure DoDisconnect;
    procedure ClearDetailsInfo;
    procedure UpdateUI;
    function ShowConnOptions: boolean;
    procedure SaveColumns(LV: TListView; const AName: string);
    procedure LoadColumns(LV: TListView; const AName: string);
    function GetTorrentError(t: TJSONObject): string;
  public
    procedure FillTorrentsList(list: TJSONArray);
    procedure UpdateTorrentsList;
    procedure FillPeersList(list: TJSONArray);
    procedure FillFilesList(list, priorities, wanted: TJSONArray);
    procedure FillGeneralInfo(t: TJSONObject);
    procedure CheckStatus(Fatal: boolean = True);
    function TorrentAction(TorrentId: integer; const AAction: string): boolean;
    function SetFilePriority(TorrentId, FileIdx: integer; const APriority: string): boolean;
    function SetCurrentFilePriority(const APriority: string): boolean;
  end;

var
  MainForm: TMainForm;
  RpcObj: TRpc;

const
  // Torrents list
  idxName = 0;
  idxSize = 1;
  idxDone = 2;
  idxStatus = 3;
  idxSeeds = 4;
  idxPeers = 5;
  idxDownSpeed = 6;
  idxUpSpeed = 7;
  idxETA = 8;
  idxRatio = 9;
  idxTorrentId = 10;
  idxTag = 11;
  idxSeedsTotal = 12;
  idxLeechers = 13;
  idxPeersTotal = 14;
  idxStateImg = 15;

  idxTorrentColCount = 16;

  // Peers list
  idxPeerIP = 0;
  idxPeerClient = 1;
  idxPeerFlags = 2;
  idxPeerDone = 3;
  idxPeerUpSpeed = 4;
  idxPeerDownSpeed = 5;

  // Files list
  idxFileName = 0;
  idxFileSize = 1;
  idxFileDone = 2;
  idxFileProgress = 3;
  idxFilePriority = 4;

implementation

uses AddTorrent, synacode, ConnOptions;

const
  TR_STATUS_CHECK_WAIT   = ( 1 shl 0 ); // Waiting in queue to check files
  TR_STATUS_CHECK        = ( 1 shl 1 ); // Checking files
  TR_STATUS_DOWNLOAD     = ( 1 shl 2 ); // Downloading
  TR_STATUS_SEED         = ( 1 shl 3 ); // Seeding
  TR_STATUS_STOPPED      = ( 1 shl 4 ); // Torrent is stopped

const
  SizeNames: array[1..5] of string = ('B', 'KB', 'MB', 'GB', 'TB');

function GetHumanSize(sz: double; RoundTo: integer = 0): string;
var
  i: integer;
begin
  i:=Low(SizeNames);
  while i <= High(SizeNames) do begin
    if sz < 1024 then
      break;
    sz:=sz/1024;
    Inc(i);
  end;
  if (RoundTo = 0) and (i > 3) then
    RoundTo:=i - 2;
  Result:=Format('%.' + IntToStr(RoundTo) + 'f %s', [sz, SizeNames[i]]);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  s: string;
  ws: TWindowState;
begin
  Application.Title:=AppName;
  Caption:=Application.Title;
  RpcObj:=TRpc.Create;
  FTorrents:=TVarList.Create(idxTorrentColCount, 0);
  s:=GetAppConfigDir(False);
  ForceDirectories(s);
  FIni:=TIniFile.Create(IncludeTrailingPathDelimiter(s)+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));
  DoDisconnect;
  PageInfo.ActivePageIndex:=0;
  PageInfoChange(nil);

  if FIni.ReadInteger('MainForm', 'State', -1) = -1 then
    Position:=poDefaultPosOnly
  else begin
    ws:=TWindowState(FIni.ReadInteger('MainForm', 'State', integer(WindowState)));
    if ws = wsNormal then begin
      Left:=FIni.ReadInteger('MainForm', 'Left', Left);
      Top:=FIni.ReadInteger('MainForm', 'Top', Top);
      Width:=FIni.ReadInteger('MainForm', 'Width', Width);
      Height:=FIni.ReadInteger('MainForm', 'Height', Height);
    end
    else
      if ws = wsMaximized then
        WindowState:=wsMaximized;
  end;

  LoadColumns(lvTorrents, 'TorrentsList');
  LoadColumns(lvFiles, 'FilesList');
  LoadColumns(lvPeers, 'PeersList');

  FTorrentsSortColumn:=FIni.ReadInteger('TorrentsList', 'SortColumn', FTorrentsSortColumn);
  FTorrentsSortDesc:=FIni.ReadBool('TorrentsList', 'SortDesc', FTorrentsSortDesc);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DoDisconnect;
  FIni.Free;
  RpcObj.Free;
  FTorrents.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  VSplitter.SetSplitterPosition(FIni.ReadInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition));
end;

procedure TMainForm.lvTorrentsColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FTorrentsSortColumn = Column.Index then
    FTorrentsSortDesc:=not FTorrentsSortDesc
  else begin
    FTorrentsSortColumn:=Column.Index;
    FTorrentsSortDesc:=False;
  end;
  UpdateTorrentsList;
end;

procedure TMainForm.lvTorrentsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
end;

procedure TMainForm.lvTorrentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if lvTorrents.Tag <> 0 then exit;
  RpcObj.Lock;
  try
    if (Item <> nil) and Selected then
      RpcObj.CurTorrentId:=ptruint(Item.Data)
    else
      RpcObj.CurTorrentId:=0;
  finally
    RpcObj.Unlock;
  end;

  ClearDetailsInfo;

  DetailsTimer.Enabled:=False;
  DetailsTimer.Enabled:=True;
end;

procedure TMainForm.acConnectExecute(Sender: TObject);
begin
  if RpcObj.Connected or (FIni.ReadString('Connection', 'Host', '') = '') then
    ShowConnOptions
  else
    DoConnect;
end;

procedure TMainForm.acConnOptionsExecute(Sender: TObject);
begin
  ShowConnOptions;
end;

procedure TMainForm.acAddTorrentExecute(Sender: TObject);
var
  req, res, args: TJSONObject;
  id: ptruint;
  t, files: TJSONArray;
  i: integer;
  s: string;
  fs: TFileStream;
begin
  if not OpenTorrentDlg.Execute then exit;
  id:=0;

  fs:=TFileStream.Create(OpenTorrentDlg.FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(s, fs.Size);
    fs.ReadBuffer(PChar(s)^, Length(s));
  finally
    fs.Free;
  end;

  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-add');
    args:=TJSONObject.Create;
    args.Add('metainfo', TJSONString.Create(EncodeBase64(s)));
    args.Add('paused', TJSONIntegerNumber.Create(1));
    req.Add('arguments', args);
    args:=nil;
    res:=RpcObj.SendRequest(req);
    if res <> nil then
      try
        args:=res.Objects['arguments'];
        if args = nil then
          raise Exception.Create('Arguments object not found.');
        id:=args.Objects['torrent-added'].Integers['id'];
      finally
        res.Free;
      end;
  finally
    req.Free;
  end;

  if id = 0 then begin
    CheckStatus(False);
    exit;
  end;

  try
    with TAddTorrentForm.Create(Self) do
    try
      args:=RpcObj.RequestInfo(id, ['files']);
      if args = nil then begin
        CheckStatus(False);
        exit;
      end;
      try
        t:=args.Arrays['torrents'];
        if t.Count = 0 then
          raise Exception.Create('Unable to get files list');
        files:=t.Objects[0].Arrays['files'];
        for i:=0 to files.Count - 1 do begin
          res:=files.Objects[i];
          with lvFiles.Items.Add do begin
            Caption:=UTF8Encode(res.Strings['name']);
            SubItems.Add(GetHumanSize(res.Floats['length']));
          end;
        end;
      finally
        args.Free;
      end;

      if ShowModal = mrOk then begin
        req:=TJSONObject.Create;
        try
          req.Add('method', 'torrent-set');
          args:=TJSONObject.Create;
          args.Add('ids', TJSONArray.Create([id]));

          files:=TJSONArray.Create;
          for i:=0 to lvFiles.Items.Count - 1 do
            if lvFiles.Items[0].Checked then
              files.Add(i);
          if files.Count > 0 then
            args.Add('files-wanted', files)
          else
            files.Free;

          files:=TJSONArray.Create;
          for i:=0 to lvFiles.Items.Count - 1 do
            if not lvFiles.Items[0].Checked then
              files.Add(i);
          if files.Count > 0 then
            args.Add('files-unwanted', files)
          else
            files.Free;

          req.Add('arguments', args);
          args:=nil;
          res:=RpcObj.SendRequest(req);
          if res = nil then begin
            CheckStatus(False);
            exit;
          end;
          res.Free;
        finally
          req.Free;
        end;

        if cbStartTorrent.Checked then
          TorrentAction(id, 'start');

        for i:=0 to lvTorrents.Items.Count - 1 do
          with lvTorrents.Items[i] do
            if ptruint(Data) = id then begin
              Selected:=True;
              MakeVisible(False);
              break;
            end;

        id:=0;
      end;
    finally
      Free;
    end;
  finally
    if id <> 0 then
      TorrentAction(id, 'remove');
  end;
end;

procedure TMainForm.acDisconnectExecute(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TMainForm.acRemoveTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  if MessageDlg('', Format('Are you sure to remove torrent ''%s''?', [lvTorrents.Selected.Caption]), mtConfirmation, mbYesNo, 0, mbNo) <> mrYes then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'remove');
end;

procedure TMainForm.acSetHighPriorityExecute(Sender: TObject);
begin
  SetCurrentFilePriority('high');
end;

procedure TMainForm.acSetLowPriorityExecute(Sender: TObject);
begin
  SetCurrentFilePriority('low');
end;

procedure TMainForm.acSetNormalPriorityExecute(Sender: TObject);
begin
  SetCurrentFilePriority('normal');
end;

procedure TMainForm.acSetNotDownloadExecute(Sender: TObject);
begin
  SetCurrentFilePriority('skip');
end;

procedure TMainForm.acStartTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'start');
end;

procedure TMainForm.acStopTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'stop');
end;

procedure TMainForm.acVerifyTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  if MessageDlg('', Format('Torrent verification may take a long time.'#13'Are you sure to start verification of torrent ''%s''?', [lvTorrents.Selected.Caption]), mtConfirmation, mbYesNo, 0, mbNo) <> mrYes then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'verify');
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateUI;
  Done:=True;
end;

procedure TMainForm.DummyTimerTimer(Sender: TObject);
begin
  if not FStarted then begin
    FStarted:=True;
    acConnect.Execute;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FIni.WriteInteger('MainForm', 'Left', Left);
  FIni.WriteInteger('MainForm', 'Top', Top);
  FIni.WriteInteger('MainForm', 'Width', Width);
  FIni.WriteInteger('MainForm', 'Height', Height);
  FIni.WriteInteger('MainForm', 'State', integer(WindowState));

  FIni.WriteInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition);

  SaveColumns(lvTorrents, 'TorrentsList');
  SaveColumns(lvFiles, 'FilesList');
  SaveColumns(lvPeers, 'PeersList');

  FIni.WriteInteger('TorrentsList', 'SortColumn', FTorrentsSortColumn);
  FIni.WriteBool('TorrentsList', 'SortDesc', FTorrentsSortDesc);
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.PageInfoChange(Sender: TObject);
begin
  RpcObj.Lock;
  try
    if PageInfo.ActivePage = tabGeneral then
      RpcObj.AdvInfo:=aiGeneral
    else
    if PageInfo.ActivePage = tabPeers then
      RpcObj.AdvInfo:=aiPeers
    else
    if PageInfo.ActivePage = tabFiles then
      RpcObj.AdvInfo:=aiFiles;
    RpcObj.RefreshNow:=True;
  finally
    RpcObj.Unlock;
  end;
end;

procedure TMainForm.DetailsTimerTimer(Sender: TObject);
begin
  DetailsTimer.Enabled:=False;
  RpcObj.RefreshNow:=True;
end;

procedure TMainForm.pmFilesPopup(Sender: TObject);
begin
  UpdateUI;
end;

procedure TMainForm.pmTorrentsPopup(Sender: TObject);
begin
  UpdateUI;
end;

procedure TMainForm.DoConnect;
var
  auth, p: string;
begin
  DoDisconnect;
  auth:=FIni.ReadString('Connection', 'UserName', '');
  if auth <> '' then begin
    p:=DecodeBase64(FIni.ReadString('Connection', 'Password', ''));
    if p <> '' then
      auth:=auth + ':' + p;
    auth:=auth + '@';
  end;
  RpcObj.Url:=Format('http://%s%s:%d/transmission/rpc', [auth, FIni.ReadString('Connection', 'Host', 'localhost'), FIni.ReadInteger('Connection', 'Port', 9091)]);
  RpcObj.RefreshInterval:=FIni.ReadInteger('Connection', 'RefreshInterval', 5);
  if RpcObj.RefreshInterval < 1 then
    RpcObj.RefreshInterval:=1;
  RpcObj.RefreshInterval:=RpcObj.RefreshInterval/SecsPerDay;
  RpcObj.InfoStatus:='Connecting to daemon...';
  CheckStatus;
  RpcObj.Connect;
end;

procedure TMainForm.DoDisconnect;

begin
  DetailsTimer.Enabled:=False;
  ClearDetailsInfo;
  lvTorrents.Clear;
  lvTorrents.Enabled:=False;
  lvTorrents.Color:=Self.Color;
  lvPeers.Enabled:=False;
  lvPeers.Color:=Self.Color;
  lvFiles.Enabled:=False;
  lvFiles.Color:=Self.Color;

  RpcObj.Disconnect;

  RpcObj.InfoStatus:='Disconnected';
  CheckStatus;
end;

procedure TMainForm.ClearDetailsInfo;
begin
  lvPeers.Clear;
  lvFiles.Clear;
  panGeneralInfo.ChildSizing.Layout:=cclNone;
  panTransfer.ChildSizing.Layout:=cclNone;
  txTotalSize.Caption:='';
  txPieces.Caption:='';
  txHash.Caption:='';
  txComment.Caption:='';
end;

procedure TMainForm.UpdateUI;
begin
  acAddTorrent.Enabled:=RpcObj.Connected;
  acStartTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acStopTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acVerifyTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acRemoveTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);

  acSetHighPriority.Enabled:=RpcObj.Connected and (lvTorrents.Selected <> nil) and
                      (lvFiles.Selected <> nil) and (PageInfo.ActivePage = tabFiles);
  acSetNormalPriority.Enabled:=acSetHighPriority.Enabled;
  acSetLowPriority.Enabled:=acSetHighPriority.Enabled;
  acSetNotDownload.Enabled:=acSetHighPriority.Enabled;
end;

function TMainForm.ShowConnOptions: boolean;
begin
  Result:=False;
  with TConnOptionsForm.Create(Self) do
  try
    edHost.Text:=FIni.ReadString('Connection', 'Host', 'localhost');
    edPort.Value:=FIni.ReadInteger('Connection', 'Port', 9091);
    edUserName.Text:=FIni.ReadString('Connection', 'UserName', '');
    if FIni.ReadString('Connection', 'Password', '') <> '' then
      edPassword.Text:='******';
    edRefreshInterval.Value:=FIni.ReadInteger('Connection', 'RefreshInterval', 5);

    if ShowModal = mrOK then begin
      if (edHost.Text <> FIni.ReadString('Connection', 'Host', 'localhost')) or
         (edPort.Value <> FIni.ReadInteger('Connection', 'Port', 9091))
      then
        DoDisconnect;

      FIni.WriteString('Connection', 'Host', edHost.Text);
      FIni.WriteInteger('Connection', 'Port', edPort.Value);
      FIni.WriteString('Connection', 'UserName', edUserName.Text);
      if edPassword.Text <> '******' then
        FIni.WriteString('Connection', 'Password', EncodeBase64(edPassword.Text));
      FIni.WriteInteger('Connection', 'RefreshInterval', edRefreshInterval.Value);

      if not RpcObj.Connected then
        DoConnect;
      Result:=True;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.SaveColumns(LV: TListView; const AName: string);
var
  i: integer;
begin
  for i:=0 to LV.Columns.Count - 1 do
    with LV.Columns[i] do begin
      FIni.WriteInteger(AName, Format('Id%d', [i]), ID);
      FIni.WriteInteger(AName, Format('Index%d', [i]), Index);
      FIni.WriteInteger(AName, Format('Width%d', [i]), Width);
    end;
end;

procedure TMainForm.LoadColumns(LV: TListView; const AName: string);
var
  i, j, ColId: integer;
begin
  LV.Columns.BeginUpdate;
  try
    for i:=0 to LV.Columns.Count - 1 do begin
      ColId:=FIni.ReadInteger(AName, Format('Id%d', [i]), -1);
      if ColId = -1 then continue;
      for j:=0 to LV.Columns.Count - 1 do
        with LV.Columns[j] do
          if ID = ColId then begin
            Index:=FIni.ReadInteger(AName, Format('Index%d', [i]), Index);
            Width:=FIni.ReadInteger(AName, Format('Width%d', [i]), Width);
            break;
          end;
    end;
  finally
    LV.Columns.EndUpdate;
  end;
end;

function TMainForm.GetTorrentError(t: TJSONObject): string;
var
  i: integer;
begin
  Result:=t.Strings['errorString'];
  if Result = '' then begin
    Result:=t.Strings['announceResponse'];
    if Result <> '' then begin
      i:=Pos('(', Result);
      if i <> 0 then
        if Copy(Result, i, 5) = '(200)' then
          Result:=''
        else
          Result:='Tracker: ' + Copy(Result, 1, i - 1);
    end;
  end;
end;

procedure TMainForm.FillTorrentsList(list: TJSONArray);
var
  i, j, row, id, StateImg: integer;
  t: TJSONObject;
  f: double;
begin
  if list = nil then begin
    FTorrents.Clear;
    exit;
  end;

  for i:=0 to FTorrents.Count - 1 do
    FTorrents[idxTag, i]:=0;

  for i:=0 to list.Count - 1 do begin
    StateImg:=-1;

    t:=list[i] as TJSONObject;
    id:=t.Integers['id'];
    row:=FTorrents.Count;
    for j:=0 to FTorrents.Count - 1 do
      if FTorrents[idxTorrentId, j] = id then begin
        row:=j;
        break;
      end;

    FTorrents[idxTorrentId, row]:=t.Integers['id'];

    if t.IndexOfName('name') >= 0 then
      FTorrents[idxName, row]:=UTF8Encode(t.Strings['name']);

    FTorrents[idxSize, row]:=t.Floats['totalSize'];
    FTorrents[idxStatus, row]:=t.Integers['status'];
    case integer(FTorrents[idxStatus, row]) of
      TR_STATUS_CHECK_WAIT: StateImg:=16;
      TR_STATUS_CHECK:      StateImg:=16;
      TR_STATUS_DOWNLOAD:   StateImg:=9;
      TR_STATUS_SEED:       StateImg:=10;
      TR_STATUS_STOPPED:    StateImg:=14;
    end;

    if FTorrents[idxStatus, row] = TR_STATUS_CHECK then
      f:=t.Floats['recheckProgress']*100.0
    else begin
      f:=t.Floats['sizeWhenDone'];
      if f <> 0 then
        f:=(f - t.Floats['leftUntilDone'])*100.0/f;
      if (StateImg = 14) and (t.Floats['leftUntilDone'] <> 0) then
        StateImg:=15;
    end;
    FTorrents[idxDone, row]:=f;

    FTorrents[idxSeeds, row]:=t.Integers['peersSendingToUs'];
    FTorrents[idxSeedsTotal, row]:=t.Integers['seeders'];
    FTorrents[idxPeers, row]:=t.Integers['peersGettingFromUs'];
    FTorrents[idxLeechers, row]:=t.Integers['leechers'];
    FTorrents[idxPeersTotal, row]:=t.Integers['peersKnown'];
    FTorrents[idxDownSpeed, row]:=t.Integers['rateDownload'];
    FTorrents[idxUpSpeed, row]:=t.Integers['rateUpload'];
    FTorrents[idxETA, row]:=t.Integers['eta'];

    if GetTorrentError(t) <> '' then
      if t.Strings['errorString'] <> '' then
        StateImg:=13
      else
        if StateImg in [9,10] then
          Inc(StateImg, 2);

    f:=t.Floats['uploadRatio'];
    if f = -2 then
      f:=MaxInt;
    FTorrents[idxRatio, row]:=f;

    FTorrents[idxStateImg, row]:=StateImg;
    FTorrents[idxTag, row]:=1;
  end;

  i:=0;
  while i < FTorrents.Count do
    if FTorrents[idxTag, i] = 0 then
      FTorrents.Delete(i)
    else
      Inc(i);

  UpdateTorrentsList;
end;

procedure TMainForm.UpdateTorrentsList;
var
  it: TListItem;

  procedure SetSubItem(idx: integer; const s: string);
  begin
    if it.SubItems.Count < idx then begin
      while it.SubItems.Count < idx - 1 do
        it.SubItems.Add('');
      it.SubItems.Add(s);
    end
    else
      it.SubItems[idx-1]:=s;
  end;

var
  i, j: integer;
  s: string;
  f: double;
begin
//  lvTorrents.BeginUpdate;
  lvTorrents.Tag:=1;
  try
    lvTorrents.Enabled:=True;
    lvTorrents.Color:=clWindow;

    FTorrents.Sort(FTorrentsSortColumn, FTorrentsSortDesc);

    for i:=0 to FTorrents.Count - 1 do begin
      if i >= lvTorrents.Items.Count then
        it:=lvTorrents.Items.Add
      else
        it:=lvTorrents.Items[i];

      it.Caption:=FTorrents[idxName, i];
      it.ImageIndex:=FTorrents[idxStateImg, i];

      SetSubItem(idxSize, GetHumanSize(FTorrents[idxSize, i], 0));

      case integer(FTorrents[idxStatus, i]) of
        TR_STATUS_CHECK_WAIT: s:='Waiting';
        TR_STATUS_CHECK:      s:='Verifying';
        TR_STATUS_DOWNLOAD:   s:='Downloading';
        TR_STATUS_SEED:       s:='Seeding';
        TR_STATUS_STOPPED:    s:='Stopped';
        else                  s:='Unknown';
      end;
      SetSubItem(idxStatus, s);

      SetSubItem(idxDone, Format('%.1f%%', [double(FTorrents[idxDone, i])]));

      j:=FTorrents[idxSeedsTotal, i];
      if j <> -1 then
        SetSubItem(idxSeeds, Format('%d/%d', [integer(FTorrents[idxSeeds, i]), j]))
      else
        SetSubItem(idxSeeds, Format('%d', [integer(FTorrents[idxSeeds, i])]));

      s:=Format('%d', [integer(FTorrents[idxPeers, i])]);
      j:=FTorrents[idxLeechers, i];
      if j <> -1 then
        s:=Format('%s/%d', [s, j]);
      j:=FTorrents[idxPeersTotal, i] - 1;
      if j >= 0 then
        s:=Format('%s (%d)', [s, j]);
      SetSubItem(idxPeers, s);

      j:=FTorrents[idxDownSpeed, i];
      if j > 0 then
        s:=GetHumanSize(j, 1) + '/s'
      else
        s:='';
      SetSubItem(idxDownSpeed, s);

      j:=FTorrents[idxUpSpeed, i];
      if j > 0 then
        s:=GetHumanSize(j, 1) + '/s'
      else
        s:='';
      SetSubItem(idxUpSpeed, s);

      j:=FTorrents[idxETA, i];
      if j = -1 then
        s:=''
      else
      if j = -2 then
        s:=''
      else begin
        if j < 60 then
          s:=Format('%ds', [j])
        else
        if j < 60*60 then
          s:=Format('%dm, %ds', [j div 60, j mod 60])
        else begin
          j:=(j + 30) div 60;
          if j < 60*24 then
            s:=Format('%dh, %dm', [j div 60, j mod 60])
          else begin
            j:=(j + 30) div 60;
            s:=Format('%dd, %dh', [j div 24, j mod 24])
          end;
        end;
      end;
      SetSubItem(idxETA, s);

      f:=FTorrents[idxRatio, i];
      if f = MaxInt then begin
        s:=Utf8Encode(WideChar($221E));
        f:=MaxInt;
      end
      else
        if f = -1 then
          s:=''
        else
          s:=Format('%.3f', [f]);
      SetSubItem(idxRatio, s);

      j:=FTorrents[idxTorrentId, i];
      if cardinal(j) = RpcObj.CurTorrentId then begin
        it.Focused:=True;
        it.Selected:=True;
      end;
      it.Data:=pointer(ptruint(j));
    end;

    while lvTorrents.Items.Count > FTorrents.Count do
      lvTorrents.Items.Delete(lvTorrents.Items.Count - 1);
  finally
    lvTorrents.Tag:=0;
//    lvTorrents.EndUpdate;
  end;
end;

procedure TMainForm.FillPeersList(list: TJSONArray);
var
  it: TListItem;

  procedure SetSubItem(idx: integer; const s: string);
  begin
    if it.SubItems.Count < idx then begin
      while it.SubItems.Count < idx - 1 do
        it.SubItems.Add('');
      it.SubItems.Add(s);
    end
    else
      it.SubItems[idx-1]:=s;
  end;

var
  i, j: integer;
  port: ptruint;
  d: TJSONData;
  p: TJSONObject;
  ports: array of pointer;
  s: string;

begin
//  lvPeers.BeginUpdate;
  try
    lvPeers.Enabled:=True;
    lvPeers.Color:=clWindow;
    if list = nil then begin
      lvPeers.Clear;
      exit;
    end;

    SetLength(ports, lvPeers.Items.Count);
    for i:=0 to lvPeers.Items.Count - 1 do begin
      ports[i]:=lvPeers.Items[i].Data;
      lvPeers.Items[i].Data:=nil;
    end;

    for i:=0 to list.Count - 1 do begin
      d:=list[i];
      if not (d is TJSONObject) then continue;
      p:=d as TJSONObject;
      s:=p.Strings['address'];
      port:=p.Integers['port'];
      it:=nil;
      for j:=0 to High(ports) do
        if (port = ptruint(ports[j])) and (lvPeers.Items[j].Caption = s) then begin
          it:=lvPeers.Items[j];
          break;
        end;
      if it = nil then
        it:=lvPeers.Items.Add;

      it.Caption:=s;

      SetSubItem(idxPeerClient, UTF8Encode(p.Strings['clientName']));

      SetSubItem(idxPeerFlags, p.Strings['flagStr']);

      SetSubItem(idxPeerDone, Format('%.1f%%', [p.Floats['progress']*100.0]));

      if p.IndexOfName('rateToClient') >= 0 then begin
        j:=p.Integers['rateToClient'];
        if j > 0 then
          s:=GetHumanSize(j, 1) + '/s'
        else
          s:='';
        SetSubItem(idxPeerDownSpeed, s);
      end;

      if p.IndexOfName('rateToPeer') >= 0 then begin
        j:=p.Integers['rateToPeer'];
        if j > 0 then
          s:=GetHumanSize(j, 1) + '/s'
        else
          s:='';
        SetSubItem(idxPeerUpSpeed, s);
      end;

      it.Data:=pointer(port);
    end;

    i:=0;
    while i < lvPeers.Items.Count do
      if lvPeers.Items[i].Data = nil then
        lvPeers.Items.Delete(i)
      else
        Inc(i);

  finally
//    lvPeers.EndUpdate;
  end;
end;

procedure TMainForm.FillFilesList(list, priorities, wanted: TJSONArray);
const
  TR_PRI_LOW    = -1;
  TR_PRI_NORMAL =  0;
  TR_PRI_HIGH   =  1;

var
  it: TListItem;

  procedure SetSubItem(idx: integer; const s: string);
  begin
    if it.SubItems.Count < idx then begin
      while it.SubItems.Count < idx - 1 do
        it.SubItems.Add('');
      it.SubItems.Add(s);
    end
    else
      it.SubItems[idx-1]:=s;
  end;

var
  i, j: integer;
  d: TJSONData;
  f: TJSONObject;
  s: string;
  ff: double;

begin
//  lvFiles.BeginUpdate;
  try
    lvFiles.Enabled:=True;
    lvFiles.Color:=clWindow;
    if list = nil then begin
      lvFiles.Clear;
      exit;
    end;

    for i:=0 to lvFiles.Items.Count - 1 do
      lvFiles.Items[i].Data:=nil;

    for i:=0 to list.Count - 1 do begin
      d:=list[i];
      if not (d is TJSONObject) then continue;
      f:=d as TJSONObject;
      s:=UTF8Encode(f.Strings['name']);
      s:=ExtractFileName(s);
      it:=nil;
      for j:=0 to lvFiles.Items.Count - 1 do
        if lvFiles.Items[j].Caption = s then begin
          it:=lvFiles.Items[j];
          break;
        end;
      if it = nil then
        it:=lvFiles.Items.Add;

      it.Caption:=s;

      ff:=f.Floats['length'];
      SetSubItem(idxFileSize, GetHumanSize(ff));
      SetSubItem(idxFileDone, GetHumanSize(f.Floats['bytesCompleted']));
      if ff <> 0 then
        ff:=f.Floats['bytesCompleted']*100.0/ff;
      SetSubItem(idxFileProgress, Format('%.1f%%', [ff]));

      if wanted.Integers[i] = 0 then
        s:='skip'
      else begin
        case priorities.Integers[i] of
          TR_PRI_LOW:    s:='low';
          TR_PRI_NORMAL: s:='normal';
          TR_PRI_HIGH:   s:='high';
          else           s:='???';
        end;
      end;
      SetSubItem(idxFilePriority, s);

      it.Data:=pointer(i + 1);
    end;

    i:=0;
    while i < lvFiles.Items.Count do
      if lvFiles.Items[i].Data = nil then
        lvFiles.Items.Delete(i)
      else
        Inc(i);

  finally
//    lvFiles.EndUpdate;
  end;
end;

procedure TMainForm.FillGeneralInfo(t: TJSONObject);
var
  i: integer;
begin
  panTransfer.ChildSizing.Layout:=cclNone;
  txError.Caption:=GetTorrentError(t);
  panTransfer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;

  panGeneralInfo.ChildSizing.Layout:=cclNone;
  txTotalSize.Caption:=Format('%s (%s done)', [GetHumanSize(t.Floats['totalSize']), GetHumanSize(t.Floats['sizeWhenDone'] - t.Floats['leftUntilDone'])]);
  if t.Floats['totalSize'] = t.Floats['haveValid'] then
    i:=t.Integers['pieceCount']
  else
    i:=Trunc(t.Floats['haveValid']/t.Floats['pieceSize']);
  txPieces.Caption:=Format('%d x %s (have %d)', [t.Integers['pieceCount'], GetHumanSize(t.Floats['pieceSize']), i]);
  txHash.Caption:=t.Strings['hashString'];
  txComment.Caption:=UTF8Encode(t.Strings['comment']);
  panGeneralInfo.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
end;

procedure TMainForm.CheckStatus(Fatal: boolean);
var
  s: string;
begin
  s:=RpcObj.Status;
  if s <> '' then begin
    RpcObj.Status:='';
    if Fatal then
      DoDisconnect;
    MessageDlg(s, mtError, [mbOK], 0);
  end;
  StatusBar.Panels[0].Text:=RpcObj.InfoStatus;
end;

function TMainForm.TorrentAction(TorrentId: integer; const AAction: string): boolean;
var
  req, res, args: TJSONObject;
begin
  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-' + AAction);
    args:=TJSONObject.Create;
    if TorrentId <> 0 then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    req.Add('arguments', args);
    res:=RpcObj.SendRequest(req);
    Result:=res <> nil;
    res.Free;
  finally
    req.Free;
  end;
  if not Result then
    CheckStatus(False)
  else
    RpcObj.RefreshNow:=True;
end;

function TMainForm.SetFilePriority(TorrentId, FileIdx: integer; const APriority: string): boolean;
var
  req, res, args: TJSONObject;
begin
  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-set');
    args:=TJSONObject.Create;
    if TorrentId <> 0 then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    if APriority = 'skip' then begin
      if FileIdx <> -1 then
        args.Add('files-unwanted', TJSONArray.Create([FileIdx]))
      else
        args.Add('files-unwanted', TJSONArray.Create);
    end
    else begin
      if FileIdx <> -1 then begin
        args.Add('files-wanted', TJSONArray.Create([FileIdx]));
        args.Add('priority-' + APriority, TJSONArray.Create([FileIdx]));
      end
      else begin
        args.Add('files-wanted', TJSONArray.Create);
        args.Add('priority-' + APriority, TJSONArray.Create);
      end;
    end;
    req.Add('arguments', args);
    res:=RpcObj.SendRequest(req);
    Result:=res <> nil;
    res.Free;
  finally
    req.Free;
  end;
  if not Result then
    CheckStatus(False)
  else
    RpcObj.RefreshNow:=True;
end;

function TMainForm.SetCurrentFilePriority(const APriority: string): boolean;
begin
  if (lvTorrents.Selected = nil) or (lvFiles.Selected = nil) or (PageInfo.ActivePage <> tabFiles) then exit;
  Result:=SetFilePriority(PtrUInt(lvTorrents.Selected.Data), PtrUInt(lvFiles.Selected.Data) - 1, APriority);
end;

initialization
  {$I main.lrs}

end.

