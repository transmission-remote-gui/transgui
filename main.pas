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
{$ifdef windows}
  Windows, win32int, InterfaceBase,
{$endif}
  Classes, SysUtils, FileUtil, zstream, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, ActnList,
  httpsend, IniFiles, StdCtrls, fpjson, jsonparser, ExtCtrls, rpc, syncobjs, variants, varlist, IpResolver,
  zipper;

const
  AppName = 'Transmission Remote GUI';
  AppVersion = '0.94 beta';

resourcestring
  SShowApp = 'Show';
  SHideApp = 'Hide';
  SAll = 'All';
  SDownloading = 'Downloading';
  SCompleted = 'Completed';
  SActive = 'Active';
  SInactive = 'Inactive';

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
    acOptions: TAction;
    acDaemonOptions: TAction;
    acStartAllTorrents: TAction;
    acStopAllTorrents: TAction;
    acExit: TAction;
    acResolveHost: TAction;
    acResolveCountry: TAction;
    acShowCountryFlag: TAction;
    acSetupColumns: TAction;
    acUpdateGeoIP: TAction;
    acTorrentProps: TAction;
    acVerifyTorrent: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    imgFlags: TImageList;
    ImageList16: TImageList;
    lvFilter: TListView;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    miToggleApp: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    panTop: TPanel;
    pmTray: TPopupMenu;
    HSplitter: TSplitter;
    pmPeers: TPopupMenu;
    TrayIcon: TTrayIcon;
    txCreated: TLabel;
    txCreatedLabel: TLabel;
    txTorrentHeader: TPanel;
    txTorrentName: TLabel;
    txTorrentNameLabel: TLabel;
    txDownProgress: TLabel;
    txDownProgressLabel: TLabel;
    panProgress: TPanel;
    pbDownloaded: TProgressBar;
    txMaxPeers: TLabel;
    txMaxPeersLabel: TLabel;
    txPeers: TLabel;
    txPeersLabel: TLabel;
    txSeeds: TLabel;
    txSeedsLabel: TLabel;
    txTrackerUpdate: TLabel;
    txTrackerUpdateLabel: TLabel;
    txRemaining: TLabel;
    txRemainingLabel: TLabel;
    txStatus: TLabel;
    txStatusLabel: TLabel;
    txRatio: TLabel;
    txRatioLabel: TLabel;
    txDownLimit: TLabel;
    txDownLimitLabel: TLabel;
    txTransferHeader: TPanel;
    txUpSpeed: TLabel;
    txUpLimit: TLabel;
    txUpSpeedLabel: TLabel;
    txDownSpeed: TLabel;
    txDownSpeedLabel: TLabel;
    txUploaded: TLabel;
    txUploadedLabel: TLabel;
    txDownloaded: TLabel;
    txDownloadedLabel: TLabel;
    txUpLimitLabel: TLabel;
    txWasted: TLabel;
    txWastedLabel: TLabel;
    miCopyLabel: TMenuItem;
    pmLabels: TPopupMenu;
    txError: TLabel;
    txErrorLabel: TLabel;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    miTools: TMenuItem;
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
    sbGenInfo: TScrollBox;
    txPieces: TLabel;
    txPiecesLabel: TLabel;
    txTotalSize: TLabel;
    txTotalSizeLabel: TLabel;
    VSplitter: TSplitter;
    StatusBar: TStatusBar;
    tabPeers: TTabSheet;
    tabGeneral: TTabSheet;
    RefreshNowTimer: TTimer;
    tabFiles: TTabSheet;
    procedure acAddTorrentExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acDisconnectExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acDaemonOptionsExecute(Sender: TObject);
    procedure acRemoveTorrentExecute(Sender: TObject);
    procedure acResolveCountryExecute(Sender: TObject);
    procedure acResolveHostExecute(Sender: TObject);
    procedure acSetHighPriorityExecute(Sender: TObject);
    procedure acSetLowPriorityExecute(Sender: TObject);
    procedure acSetNormalPriorityExecute(Sender: TObject);
    procedure acSetNotDownloadExecute(Sender: TObject);
    procedure acSetupColumnsExecute(Sender: TObject);
    procedure acShowCountryFlagExecute(Sender: TObject);
    procedure acStartAllTorrentsExecute(Sender: TObject);
    procedure acStartTorrentExecute(Sender: TObject);
    procedure acStopAllTorrentsExecute(Sender: TObject);
    procedure acStopTorrentExecute(Sender: TObject);
    procedure acTorrentPropsExecute(Sender: TObject);
    procedure acUpdateGeoIPExecute(Sender: TObject);
    procedure acVerifyTorrentExecute(Sender: TObject);
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationPropertiesMinimize(Sender: TObject);
    procedure ApplicationPropertiesRestore(Sender: TObject);
    procedure DummyTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilterCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvFilterResize(Sender: TObject);
    procedure lvFilterSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvTorrentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvTorrentsDblClick(Sender: TObject);
    procedure lvTorrentsResize(Sender: TObject);
    procedure lvTorrentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miAboutClick(Sender: TObject);
    procedure miCopyLabelClick(Sender: TObject);
    procedure miToggleAppClick(Sender: TObject);
    procedure PageInfoChange(Sender: TObject);
    procedure RefreshNowTimerTimer(Sender: TObject);
    procedure pmFilesPopup(Sender: TObject);
    procedure pmTorrentsPopup(Sender: TObject);
    procedure sbGenInfoResize(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FIni: TIniFile;
    FStarted: boolean;
    FTorrents: TVarList;
    FTorrentsSortColumn: integer;
    FTorrentsSortDesc: boolean;
    FTrackers: TStringList;
    FResolver: TIpResolver;
    FUnZip: TUnZipper;

    procedure DoConnect;
    procedure DoDisconnect;
    procedure UpdateUI;
    function ShowConnOptions: boolean;
    procedure SaveColumns(LV: TListView; const AName: string; FullInfo: boolean = False);
    procedure LoadColumns(LV: TListView; const AName: string; FullInfo: boolean = False);
    function GetTorrentError(t: TJSONObject): string;
    function SecondsToString(j: integer): string;
    procedure DoAddTorrent(const FileName: Utf8String);
    procedure UpdateTray;
    procedure HideApp;
    procedure ShowApp;
    procedure DownloadFinished(const TorrentName: string);
    procedure SelectFilterItem(Data: PtrInt);
    function GetFlagImage(const CountryCode: string): integer;
    procedure BeforeCloseApp;
    function GetGeoIpDatabase: string;
    function GetFlagsArchive: string;
    function DownloadGeoIpDatabase(AUpdate: boolean): boolean;
    procedure TorrentColumnsChanged;
    function EtaToString(ETA: integer): string;
    function GetTorrentStatus(TorrentIdx: integer): string;
    function GetSeedsText(Seeds, SeedsTotal: integer): string;
    function GetPeersText(Peers, PeersTotal, Leechers: integer): string;
    function RatioToString(Ratio: double): string;
    function TorrentDateTimeToString(d: Int64): string;
  public
    procedure FillTorrentsList(list: TJSONArray);
    procedure UpdateTorrentsList;
    procedure FillPeersList(list: TJSONArray);
    procedure FillFilesList(list, priorities, wanted: TJSONArray);
    procedure FillGeneralInfo(t: TJSONObject);
    procedure CheckStatus(Fatal: boolean = True);
    function TorrentAction(TorrentId: integer; const AAction: string): boolean;
    function SetFilePriority(TorrentId: integer; const Files: array of integer; const APriority: string): boolean;
    function SetCurrentFilePriority(const APriority: string): boolean;
    procedure ClearDetailsInfo;
  end;

function CheckAppParams: boolean;
function GetHumanSize(sz: double; RoundTo: integer = 0): string;

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
  idxDownloaded = 10;
  idxUploaded = 11;
  idxTracker = 12;
  idxTrackerStatus = 13;
  idxAddedOn = 14;
  idxCompletedOn = 15;
  idxLastActive = 16;

  idxLastVisible = 16;
  idxTorrentId = idxLastVisible + 1;
  idxTag = idxLastVisible + 2;
  idxSeedsTotal = idxLastVisible + 3;
  idxLeechers = idxLastVisible + 4;
  idxPeersTotal = idxLastVisible + 5;
  idxStateImg = idxLastVisible + 6;

  idxTorrentColCount = idxStateImg + 1;

  // Peers list
  idxPeerIP = 0;
  idxPeerCountry = 1;
  idxPeerClient = 2;
  idxPeerFlags = 3;
  idxPeerDone = 4;
  idxPeerUpSpeed = 5;
  idxPeerDownSpeed = 6;

  // Files list
  idxFileName = 0;
  idxFileSize = 1;
  idxFileDone = 2;
  idxFileProgress = 3;
  idxFilePriority = 4;

  // Filter idices
  fltAll      = 0;
  fltDown     = 1;
  fltDone     = 2;
  fltActive   = 3;
  fltInactive = 4;

  // Status images
  imgDown      = 9;
  imgSeed      = 10;
  imgDownError = 11;
  imgSeedError = 12;
  imgError     = 13;
  imgDone      = 14;
  imgStopped   = 15;
  imgDownQueue = 16;
  imgSeedQueue = 17;
  imgAll       = 19;
  imgActive    = 20;

  StatusFiltersCount = 5;

  TorrentFieldsMap: array[idxName..idxLastActive] of string =
    ('', 'totalSize', '', 'status', 'peersSendingToUs,seeders',
     'peersGettingFromUs,leechers,peersKnown', 'rateDownload', 'rateUpload', 'eta', 'uploadRatio',
     'downloadedEver', 'uploadedEver', '', '', 'addedDate', 'doneDate', 'activityDate');

implementation

uses
  AddTorrent, synacode, ConnOptions, clipbrd, DateUtils, tz, TorrProps, DaemonOptions, About,
  ToolWin, download, ColSetup;

const
  TR_STATUS_CHECK_WAIT   = ( 1 shl 0 ); // Waiting in queue to check files
  TR_STATUS_CHECK        = ( 1 shl 1 ); // Checking files
  TR_STATUS_DOWNLOAD     = ( 1 shl 2 ); // Downloading
  TR_STATUS_SEED         = ( 1 shl 3 ); // Seeding
  TR_STATUS_STOPPED      = ( 1 shl 4 ); // Torrent is stopped

  TR_SPEEDLIMIT_GLOBAL    = 0;    // only follow the overall speed limit
  TR_SPEEDLIMIT_SINGLE    = 1;    // only follow the per-torrent limit
  TR_SPEEDLIMIT_UNLIMITED = 2;    // no limits at all

const
  SizeNames: array[1..5] of string = ('b', 'KB', 'MB', 'GB', 'TB');

function GetHumanSize(sz: double; RoundTo: integer = 0): string;
var
  i: integer;
begin
  i:=Low(SizeNames);
  if RoundTo > 0 then begin
    Inc(i);
    sz:=sz/1024;
  end;
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

var
  BusyCount: integer = 0;

procedure AppBusy;
begin
  Inc(BusyCount);
  Screen.Cursor:=crHourGlass;
end;

procedure AppNormal;
begin
  Dec(BusyCount);
  if BusyCount <= 0 then begin
    BusyCount:=0;
    Screen.Cursor:=crDefault;
  end;
end;

procedure ForceAppNormal;
begin
  BusyCount:=0;
  AppNormal;
end;

function AddToChannel(Clr: TColor; Value: integer; Position: byte): TColor;
var i: integer;

begin
     i:=(Clr shr (Position*8)) and $FF;
     i:=i + Value;
     if i < 0 then i:=0;
     if i > $FF then i:=$FF;
     Result:=Clr and (not (Cardinal($FF) shl (Position*8))) or (Cardinal(i) shl (Position*8));
end;

function AddToColor(Color: TColor; R, G, B: integer): TColor;
begin
     Result:=ColorToRGB(Color);
     Result:=AddToChannel(Result, R, 0);
     Result:=AddToChannel(Result, G, 1);
     Result:=AddToChannel(Result, B, 2);
end;

function GetLikeColor(Color: TColor; Delta: integer): TColor;
var i, j: integer;

begin
    Result:=ColorToRGB(Color);
    j:=Result and $FF;               //red
    i:=(Result shr 8) and $FF;       // green
    if i > j then
      j:=i;
    i:=((Result shr 16) and $FF) shr 1;      // blue
    if i > j then
      j:=i;
    if j < $80 then
      i:=(($80 - j) div $20 + 1)*Delta
    else
      i:=Delta;
    if (i + j > 255) or (i + j < 0) then
      i:=-Delta;

    Result:=AddToColor(Result, i, i, i);
end;

function LocateFile(const FileName: string; const Paths: array of string): string;
var
  i: integer;
begin
  for i:=Low(Paths) to High(Paths) do begin
    Result:=IncludeTrailingPathDelimiter(Paths[i]) + FileName;
    if FileExists(Result) then
      exit;
  end;
  Result:='';
end;

var
  FHomeDir: string;
  FIPCFileName: string;
  FRunFileName: string;

function CheckAppParams: boolean;
var
  h: THandle;
  s: utf8string;
  i: integer;
begin
  Application.Title:=AppName;
  FHomeDir:=IncludeTrailingPathDelimiter(GetAppConfigDir(False));
  ForceDirectories(FHomeDir);
  FIPCFileName:=FHomeDir + 'ipc.txt';
  FRunFileName:=FHomeDir + 'run';

  if ParamCount > 0 then begin
    s:=ParamStrUTF8(1);
    if FileExistsUTF8(s) then begin
      h:=FileCreate(FIPCFileName, fmCreate);
      if h <> THandle(-1) then begin
        FileWrite(h, s[1], Length(s));
        FileClose(h);
      end;
    end;
  end;

  if FileExists(FRunFileName) then begin
    if not FileExists(FIPCFileName) then
      FileClose(FileCreate(FIPCFileName, fmCreate));
    for i:=1 to 10 do
      if not FileExists(FIPCFileName) then begin
        Result:=False;
        exit;
      end
      else
        Sleep(200);
  end
  else
    FileClose(FileCreate(FRunFileName, fmCreate));

  Result:=True;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  ws: TWindowState;
begin
  Application.Title:=AppName;
  Caption:=Application.Title;
  TrayIcon.Icon.Assign(Application.Icon);
  RpcObj:=TRpc.Create;
  FTorrents:=TVarList.Create(idxTorrentColCount, 0);
  FIni:=TIniFile.Create(FHomeDir+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));
  FTrackers:=TStringList.Create;

  with lvFilter.Items do begin
    Add.ImageIndex:=imgAll;
    Add.ImageIndex:=imgDown;
    Add.ImageIndex:=imgSeed;
    Add.ImageIndex:=imgActive;
    Add.ImageIndex:=imgStopped;
  end;

  DoDisconnect;
  PageInfo.ActivePageIndex:=0;
  PageInfoChange(nil);
{$ifdef LCLgtk2}
  with MainToolBar do begin
    EdgeBorders:=[ebLeft, ebTop, ebRight, ebBottom];
    EdgeInner:=esNone;
    EdgeOuter:=esRaised;
    Flat:=True;
  end;
{$endif}
  txTransferHeader.Color:=GetLikeColor(clBtnFace, -15);
  txTorrentHeader.Color:=txTransferHeader.Color;
  txTransferHeader.Caption:=' ' + txTransferHeader.Caption;
  txTorrentHeader.Caption:=' ' + txTorrentHeader.Caption;

  if FIni.ReadInteger('MainForm', 'State', -1) = -1 then
    Position:=poScreenCenter
  else begin
    ws:=TWindowState(FIni.ReadInteger('MainForm', 'State', integer(WindowState)));
    Left:=FIni.ReadInteger('MainForm', 'Left', Left);
    Top:=FIni.ReadInteger('MainForm', 'Top', Top);
    Width:=FIni.ReadInteger('MainForm', 'Width', Width);
    Height:=FIni.ReadInteger('MainForm', 'Height', Height);
    if ws = wsMaximized then
      WindowState:=wsMaximized;
  end;

  LoadColumns(lvTorrents, 'TorrentsList', True);
  TorrentColumnsChanged;
  LoadColumns(lvFiles, 'FilesList');
  LoadColumns(lvPeers, 'PeersList');

  FTorrentsSortColumn:=FIni.ReadInteger('TorrentsList', 'SortColumn', FTorrentsSortColumn);
  FTorrentsSortDesc:=FIni.ReadBool('TorrentsList', 'SortDesc', FTorrentsSortDesc);

  acResolveHost.Checked:=FIni.ReadBool('PeersList', 'ResolveHost', True);
  acResolveCountry.Checked:=FIni.ReadBool('PeersList', 'ResolveCountry', True) and (GetGeoIpDatabase <> '');
  acShowCountryFlag.Checked:=FIni.ReadBool('PeersList', 'ShowCountryFlag', True) and (GetFlagsArchive <> '');
  acShowCountryFlag.Enabled:=acResolveCountry.Checked;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DeleteFile(FRunFileName);
  FResolver.Free;
  FIni.Free;
  FTorrents.Free;
  FTrackers.Free;
  FUnZip.Free;
  RpcObj.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  VSplitter.SetSplitterPosition(FIni.ReadInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition));
  HSplitter.SetSplitterPosition(FIni.ReadInteger('MainForm', 'HSplitter', HSplitter.GetSplitterPosition));
  DummyTimer.Enabled:=True;
  UpdateTray;
end;

procedure TMainForm.lvFilterCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
{
var
  i: integer;
  R: TRect;
}
begin
  DefaultDraw:=Item.Data = nil;
  if DefaultDraw then exit;
{
  with lvFilter.Canvas do begin
    Brush.Color:=lvFilter.Color;
    R:=Item.DisplayRect(drBounds);
    FillRect(R);
    i:=(R.Bottom + R.Top) div 2;
    Brush.Color:=clBtnFace;
    FillRect(Rect(4, i - 1, Sender.ClientWidth - 4, i + 1));
  end;
}
end;

procedure TMainForm.lvFilterResize(Sender: TObject);
begin
  lvFilter.Columns[0].Width:=lvFilter.ClientWidth - 1;
end;

procedure TMainForm.lvFilterSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if lvFilter.Tag <> 0 then exit;
  if Selected and (ptrint(ptruint(Item.Data)) = -1) then begin
    Application.QueueAsyncCall(@SelectFilterItem, Item.Index - 1);
    exit;
  end;
  RefreshNowTimer.Enabled:=False;
  RefreshNowTimer.Enabled:=True;
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

procedure TMainForm.lvTorrentsDblClick(Sender: TObject);
begin
  acTorrentProps.Execute;
end;

procedure TMainForm.lvTorrentsResize(Sender: TObject);
begin
  if not FStarted then begin
    VSplitter.SetSplitterPosition(FIni.ReadInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition));
    HSplitter.SetSplitterPosition(FIni.ReadInteger('MainForm', 'HSplitter', HSplitter.GetSplitterPosition));
  end;
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

  RefreshNowTimer.Enabled:=False;
  RefreshNowTimer.Enabled:=True;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
  with TAboutForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.miCopyLabelClick(Sender: TObject);
begin
  with TLabel(pmLabels.PopupComponent) do
    if (Length(Name) > 5) and (Copy(Name, Length(Name) - 4, 5) = 'Label') then
      Clipboard.AsText:=TLabel(Parent.ControlByName(Copy(Name, 1, Length(Name) - 5))).Caption
    else
      Clipboard.AsText:=Caption;
end;

procedure TMainForm.acConnectExecute(Sender: TObject);
begin
  if RpcObj.Connected or (FIni.ReadString('Connection', 'Host', '') = '') then
    ShowConnOptions
  else
    DoConnect;
end;

procedure TMainForm.acOptionsExecute(Sender: TObject);
begin
  ShowConnOptions;
end;

procedure TMainForm.acAddTorrentExecute(Sender: TObject);
begin
  if not OpenTorrentDlg.Execute then exit;
  DoAddTorrent(OpenTorrentDlg.FileName);
end;

procedure TMainForm.DoAddTorrent(const FileName: Utf8String);
var
  torrent: string;

  function _AddTorrent(args: TJSONObject): integer;
  var
    req: TJSONObject;
  begin
    Result:=0;
    req:=TJSONObject.Create;
    try
      req.Add('method', 'torrent-add');
      args.Add('metainfo', TJSONString.Create(torrent));
      req.Add('arguments', args);
      args:=RpcObj.SendRequest(req);
      if args <> nil then
      try
        Result:=args.Objects['torrent-added'].Integers['id'];
      finally
        args.Free;
      end;
    finally
      req.Free;
    end;

    if Result = 0 then
      CheckStatus(False);
  end;

var
  req, res, args: TJSONObject;
  id: ptruint;
  t, files: TJSONArray;
  i, j: integer;
  fs: TFileStream;
  s, OldDownloadDir, IniSec: string;
  tt: TDateTime;
begin
  AppBusy;
  id:=0;

  fs:=TFileStream.Create(UTF8Decode(FileName), fmOpenRead or fmShareDenyNone);
  try
    SetLength(torrent, fs.Size);
    fs.ReadBuffer(PChar(torrent)^, Length(torrent));
  finally
    fs.Free;
  end;
  torrent:=EncodeBase64(torrent);
  try
    with TAddTorrentForm.Create(Self) do
    try
      IniSec:='AddTorrent.' + RpcObj.Http.TargetHost;
      j:=FIni.ReadInteger(IniSec, 'FolderCount', 0);
      for i:=0 to j - 1 do begin
        s:=FIni.ReadString(IniSec, Format('Folder%d', [i]), '');
        if s <> '' then
          cbDestFolder.Items.Add(s);
      end;
      if cbDestFolder.Items.Count > 0 then
        cbDestFolder.ItemIndex:=0;

      if cbDestFolder.Text = '' then begin
        req:=TJSONObject.Create;
        try
          req.Add('method', 'session-get');
          args:=RpcObj.SendRequest(req);
          if args = nil then begin
            CheckStatus(False);
            exit;
          end;
          cbDestFolder.Items.Add(UTF8Encode(args.Strings['download-dir']));
          cbDestFolder.ItemIndex:=0;
          args.Free;
        finally
          req.Free;
        end;
      end;

      lvFilter.Items[0].Selected:=True;

      args:=TJSONObject.Create;
      args.Add('paused', TJSONIntegerNumber.Create(1));
      i:=FIni.ReadInteger(IniSec, 'PeerLimit', 0);
      if i <> 0 then
        args.Add('peer-limit', TJSONIntegerNumber.Create(i));
      args.Add('download-dir', TJSONString.Create(UTF8Decode(cbDestFolder.Text)));
      id:=_AddTorrent(args);
      if id = 0 then
        exit;

      RpcObj.RefreshNow:=True;

      args:=RpcObj.RequestInfo(id, ['files', 'maxConnectedPeers']);
      if args = nil then begin
        CheckStatus(False);
        exit;
      end;
      try
        t:=args.Arrays['torrents'];
        if t.Count = 0 then
          raise Exception.Create('Unable to get files list');
        edPeerLimit.Value:=t.Objects[0].Integers['maxConnectedPeers'];
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

      OldDownloadDir:=cbDestFolder.Text;
      AppNormal;

      if ShowModal = mrOk then begin
        AppBusy;
        Self.Update;

        if OldDownloadDir <> cbDestFolder.Text then begin
          TorrentAction(id, 'remove');
          id:=0;
          args:=TJSONObject.Create;
          args.Add('paused', TJSONIntegerNumber.Create(1));
          args.Add('peer-limit', TJSONIntegerNumber.Create(edPeerLimit.Value));
          args.Add('download-dir', TJSONString.Create(UTF8Decode(cbDestFolder.Text)));
          id:=_AddTorrent(args);
          if id = 0 then
            exit;
          RpcObj.RefreshNow:=True;
          Application.ProcessMessages;
        end;

        req:=TJSONObject.Create;
        try
          req.Add('method', 'torrent-set');
          args:=TJSONObject.Create;
          args.Add('ids', TJSONArray.Create([id]));
          args.Add('peer-limit', TJSONIntegerNumber.Create(edPeerLimit.Value));

          files:=TJSONArray.Create;
          for i:=0 to lvFiles.Items.Count - 1 do
            if lvFiles.Items[i].Checked then
              files.Add(i);
          if files.Count > 0 then
            args.Add('files-wanted', files)
          else
            files.Free;

          files:=TJSONArray.Create;
          for i:=0 to lvFiles.Items.Count - 1 do
            if not lvFiles.Items[i].Checked then
              files.Add(i);
          if files.Count > 0 then
            args.Add('files-unwanted', files)
          else
            files.Free;

          req.Add('arguments', args);
          args:=nil;
          args:=RpcObj.SendRequest(req, False);
          if args = nil then begin
            CheckStatus(False);
            exit;
          end;
          args.Free;
        finally
          req.Free;
        end;

        if cbStartTorrent.Checked then
          TorrentAction(id, 'start');

        tt:=Now;
        while (Now - tt < 2/SecsPerDay) and (id <> 0) do begin
          Application.ProcessMessages;
          for i:=0 to lvTorrents.Items.Count - 1 do
            with lvTorrents.Items[i] do
              if ptruint(Data) = id then begin
                Selected:=True;
                MakeVisible(False);
                RpcObj.CurTorrentId:=id;
                lvTorrents.SetFocus;
                id:=0;
                break;
              end;
          Sleep(100);
        end;

        id:=0;

        FIni.WriteInteger(IniSec, 'PeerLimit', edPeerLimit.Value);
        i:=cbDestFolder.Items.IndexOf(cbDestFolder.Text);
        if i >= 0 then
          cbDestFolder.Items.Move(i, 0)
        else
          cbDestFolder.Items.Insert(0, cbDestFolder.Text);
        while cbDestFolder.Items.Count > 6 do
          cbDestFolder.Items.Delete(cbDestFolder.Items.Count - 1);
        FIni.WriteInteger(IniSec, 'FolderCount', cbDestFolder.Items.Count);
        for i:=0 to cbDestFolder.Items.Count - 1 do
          FIni.WriteString(IniSec, Format('Folder%d', [i]), cbDestFolder.Items[i]);
        AppNormal;
      end;
    finally
      Free;
    end;
  finally
    if id <> 0 then
      TorrentAction(id, 'remove');
  end;
end;

procedure TMainForm.UpdateTray;
begin
  TrayIcon.Visible:=not Self.Visible or (WindowState = wsMinimized) or FIni.ReadBool('Interface', 'TrayIconAlways', True);
  if Visible and (WindowState <> wsMinimized) then
    miToggleApp.Caption:=SHideApp
  else
    miToggleApp.Caption:=SShowApp;
end;

procedure TMainForm.HideApp;
begin
  if WindowState <> wsMinimized then
    Hide;
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_HIDE);
{$endif mswindows}
end;

procedure TMainForm.ShowApp;
begin
{$ifdef mswindows}
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_SHOW);
{$endif mswindows}
  if WindowState = wsMinimized then
    Application.Restore;
  Show;
  Application.BringToFront;
  BringToFront;
end;

procedure TMainForm.DownloadFinished(const TorrentName: string);
begin
  if not TrayIcon.Visible then exit;
  TrayIcon.BalloonHint:=Format('''%s'' has finished downloading', [TorrentName]);
  TrayIcon.BalloonTitle:='Download complete';
  TrayIcon.ShowBalloonHint;
end;

procedure TMainForm.SelectFilterItem(Data: PtrInt);
begin
  if Data < lvFilter.Items.Count then
    lvFilter.Items[Data].Selected:=True;
end;

function TMainForm.GetFlagImage(const CountryCode: string): integer;
var
  s, FlagsPath, ImageName: string;
  pic: TPicture;
begin
  Result:=0;
  if CountryCode = '' then exit;
  try
    ImageName:=CountryCode + '.png';
    FlagsPath:=FHomeDir + 'flags' + DirectorySeparator;
    if not FileExists(FlagsPath + ImageName) then begin
      // Unzipping flag image
      if FUnZip = nil then begin
        s:=GetFlagsArchive;
        if s <> '' then begin
          ForceDirectories(FlagsPath);
          FUnZip:=TUnZipper.Create;
          FUnZip.OutputPath:=FlagsPath;
          FUnZip.FileName:=s;
        end
        else
          exit;
      end;

      FUnZip.Files.Clear;
      FUnZip.Files.Add(ImageName);
      try
        FUnZip.UnZipAllFiles;
      except
        FreeAndNil(FUnZip);
        DeleteFile(GetFlagsArchive);
        acShowCountryFlag.Checked:=False;
        MessageDlg('Unable to extract flag image.'+LineEnding+Exception(ExceptObject).Message, mtError, [mbOK], 0);
        exit;
      end;
      if not FileExists(FlagsPath + ImageName) then exit;
    end;

    pic:=TPicture.Create;
    try
      pic.LoadFromFile(FlagsPath + ImageName);
      if imgFlags.Count = 1 then begin
        imgFlags.Width:=pic.Width;
        imgFlags.Height:=pic.Height;
      end;
      Result:=imgFlags.AddMasked(pic.Bitmap, clNone);
    finally
      pic.Free;
    end;
  except
  end;
end;

procedure TMainForm.BeforeCloseApp;
begin
  if WindowState = wsNormal then begin
    FIni.WriteInteger('MainForm', 'Left', Left);
    FIni.WriteInteger('MainForm', 'Top', Top);
    FIni.WriteInteger('MainForm', 'Width', Width);
    FIni.WriteInteger('MainForm', 'Height', Height);
  end;
  if WindowState <> wsMinimized then
    FIni.WriteInteger('MainForm', 'State', integer(WindowState));

  FIni.WriteInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition);
  FIni.WriteInteger('MainForm', 'HSplitter', HSplitter.GetSplitterPosition);

  SaveColumns(lvTorrents, 'TorrentsList', True);
  SaveColumns(lvFiles, 'FilesList');
  SaveColumns(lvPeers, 'PeersList');

  FIni.WriteInteger('TorrentsList', 'SortColumn', FTorrentsSortColumn);
  FIni.WriteBool('TorrentsList', 'SortDesc', FTorrentsSortDesc);

  FIni.WriteBool('PeersList', 'ResolveHost', acResolveHost.Checked);
  FIni.WriteBool('PeersList', 'ResolveCountry', acResolveCountry.Checked);
  FIni.WriteBool('PeersList', 'ShowCountryFlag', acShowCountryFlag.Checked);

  DoDisconnect;
  Application.ProcessMessages;
end;

function TMainForm.GetGeoIpDatabase: string;
begin
  Result:=LocateFile('GeoIP.dat', [FHomeDir, ExtractFilePath(ParamStr(0))]);
end;

function TMainForm.GetFlagsArchive: string;
begin
  Result:=LocateFile('flags.zip', [FHomeDir, ExtractFilePath(ParamStr(0))]);
end;

function TMainForm.DownloadGeoIpDatabase(AUpdate: boolean): boolean;
const
  GeoLiteURL = 'http://geolite.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz';
var
  tmp: string;
  gz: TGZFileStream;
  fs: TFileStream;
begin
  Result:=False;
  tmp:=FHomeDir + 'GeoIP.dat.gz';
  if not FileExists(tmp) or AUpdate then begin
    if MessageDlg('', 'Geo IP database is needed to resolve country by IP address.' + LineEnding + 'Download this database now?', mtConfirmation, mbYesNo, 0, mbYes) <> mrYes then
      exit;
    if not DownloadFile(GeoLiteURL, ExtractFilePath(tmp), ExtractFileName(tmp)) then
      exit;
  end;
  try
    gz:=TGZFileStream.Create(tmp, gzopenread);
    try
      fs:=TFileStream.Create(FHomeDir + 'GeoIP.dat', fmCreate);
      try
        while fs.CopyFrom(gz, 64*1024) = 64*1024 do
          ;
      finally
        fs.Free;
      end;
    finally
      gz.Free;
    end;
    DeleteFile(tmp);
  except
    DeleteFile(FHomeDir + 'GeoIP.dat');
    DeleteFile(tmp);
    raise;
  end;
  FreeAndNil(FResolver);
  Result:=True;
end;

procedure TMainForm.TorrentColumnsChanged;
var
  i: integer;
  s: string;
begin
  s:='';
  for i:=0 to lvTorrents.Columns.Count - 1 do
    with lvTorrents.Columns[i] do
      if Visible and (Width > 0) then begin
        if s <> '' then
          s:=s + ',';
        s:=s + TorrentFieldsMap[ID];
      end;
  RpcObj.TorrentFields:=s;
  RpcObj.RefreshNow:=True;
end;

function TMainForm.EtaToString(ETA: integer): string;
begin
  if ETA = -1 then
    Result:=''
  else
  if ETA = -2 then
    Result:=''
  else
    Result:=SecondsToString(ETA);
end;

function TMainForm.GetTorrentStatus(TorrentIdx: integer): string;
begin
  case integer(FTorrents[idxStatus, TorrentIdx]) of
    TR_STATUS_CHECK_WAIT:
      Result:='Waiting';
    TR_STATUS_CHECK:
      Result:='Verifying';
    TR_STATUS_DOWNLOAD:
      Result:='Downloading';
    TR_STATUS_SEED:
      Result:='Seeding';
    TR_STATUS_STOPPED:
      if FTorrents[idxStateImg, TorrentIdx] = imgDone then
        Result:='Finished'
      else
        Result:='Stopped';
    else
      Result:='Unknown';
  end;
end;

function TMainForm.GetSeedsText(Seeds, SeedsTotal: integer): string;
begin
  if SeedsTotal <> -1 then
    Result:=Format('%d/%d', [Seeds, SeedsTotal])
  else
    Result:=Format('%d', [Seeds]);
end;

function TMainForm.GetPeersText(Peers, PeersTotal, Leechers: integer): string;
begin
  Result:=Format('%d', [Peers]);
  if Leechers <> -1 then
    Result:=Format('%s/%d', [Result, Leechers]);
  Dec(PeersTotal);
  if PeersTotal >= 0 then
    Result:=Format('%s (%d)', [Result, PeersTotal]);
end;

function TMainForm.RatioToString(Ratio: double): string;
begin
  if (Ratio = MaxInt) or (Ratio = -2) then
    Result:=Utf8Encode(WideChar($221E))
  else
    if Ratio = -1 then
      Result:=''
    else
      Result:=Format('%.3f', [Ratio]);
end;

function TMainForm.TorrentDateTimeToString(d: Int64): string;
begin
  if d = 0 then
    Result:=''
  else
    Result:=DateTimeToStr(UnixToDateTime(d) + GetTimeZoneDelta);
end;

procedure TMainForm.acDisconnectExecute(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  BeforeCloseApp;
  Application.Terminate;
end;

procedure TMainForm.acDaemonOptionsExecute(Sender: TObject);
var
  req, args: TJSONObject;
  s: string;
begin
  with TDaemonOptionsForm.Create(Self) do
  try
    AppBusy;
    req:=TJSONObject.Create;
    try
      req.Add('method', 'session-get');
      args:=RpcObj.SendRequest(req);
      if args <> nil then
        try
          edDownloadDir.Text:=UTF8Encode(args.Strings['download-dir']);
          edPort.Value:=args.Integers['port'];
          cbPortForwarding.Checked:=args.Integers['port-forwarding-enabled'] <> 0;
          cbPEX.Checked:=args.Integers['pex-allowed'] <> 0;
          s:=args.Strings['encryption'];
          if s = 'preferred' then
            cbEncryption.ItemIndex:=1
          else
          if s = 'required' then
            cbEncryption.ItemIndex:=2
          else
            cbEncryption.ItemIndex:=0;
          edMaxPeers.Value:=args.Integers['peer-limit'];
          cbMaxDown.Checked:=args.Integers['speed-limit-down-enabled'] <> 0;
          edMaxDown.Value:=args.Integers['speed-limit-down'];
          cbMaxUp.Checked:=args.Integers['speed-limit-up-enabled'] <> 0;
          edMaxUp.Value:=args.Integers['speed-limit-up'];
        finally
          args.Free;
        end
      else begin
        CheckStatus(False);
        exit;
      end;
    finally
      req.Free;
    end;
    cbMaxDownClick(nil);
    cbMaxUpClick(nil);
    AppNormal;

    if ShowModal = mrOK then begin
      AppBusy;
      Self.Update;
      req:=TJSONObject.Create;
      try
        req.Add('method', 'session-set');
        args:=TJSONObject.Create;
        args.Add('download-dir', TJSONString.Create(UTF8Decode(edDownloadDir.Text)));
        args.Add('port', TJSONIntegerNumber.Create(edPort.Value));
        args.Add('port-forwarding-enabled', TJSONIntegerNumber.Create(integer(cbPortForwarding.Checked) and 1));
        args.Add('pex-allowed', TJSONIntegerNumber.Create(integer(cbPEX.Checked) and 1));
        case cbEncryption.ItemIndex of
          1: s:='preferred';
          2: s:='required';
          else s:='tolerated';
        end;
        args.Add('encryption', TJSONString.Create(s));
        args.Add('peer-limit', TJSONIntegerNumber.Create(edMaxPeers.Value));
        args.Add('speed-limit-down-enabled', TJSONIntegerNumber.Create(integer(cbMaxDown.Checked) and 1));
        if cbMaxDown.Checked then
          args.Add('speed-limit-down', TJSONIntegerNumber.Create(edMaxDown.Value));
        args.Add('speed-limit-up-enabled', TJSONIntegerNumber.Create(integer(cbMaxUp.Checked) and 1));
        if cbMaxUp.Checked then
          args.Add('speed-limit-up', TJSONIntegerNumber.Create(edMaxUp.Value));
        req.Add('arguments', args);
        args:=RpcObj.SendRequest(req, False);
        if args = nil then begin
          CheckStatus(False);
          exit;
        end;
        args.Free;
      finally
        req.Free;
      end;
      AppNormal;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.acRemoveTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  if MessageDlg('', Format('Are you sure to remove torrent ''%s''?', [lvTorrents.Selected.Caption]), mtConfirmation, mbYesNo, 0, mbNo) <> mrYes then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'remove');
end;

procedure TMainForm.acResolveCountryExecute(Sender: TObject);
begin
  if not acResolveCountry.Checked then
    if GetGeoIpDatabase = '' then
      if not DownloadGeoIpDatabase(False) then
        exit;

  acResolveCountry.Checked:=not acResolveCountry.Checked;
  FreeAndNil(FResolver);
  RpcObj.RefreshNow:=True;
  acShowCountryFlag.Enabled:=acResolveCountry.Checked;
end;

procedure TMainForm.acResolveHostExecute(Sender: TObject);
begin
  acResolveHost.Checked:=not acResolveHost.Checked;
  FreeAndNil(FResolver);
  RpcObj.RefreshNow:=True;
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

procedure TMainForm.acSetupColumnsExecute(Sender: TObject);
begin
  if not SetupColumns(lvTorrents) then exit;
  TorrentColumnsChanged;
end;

procedure TMainForm.acShowCountryFlagExecute(Sender: TObject);
const
  FlagsURL = 'http://transmisson-remote-gui.googlecode.com/files/flags.zip';
begin
  if not acShowCountryFlag.Checked then
    if GetFlagsArchive = '' then begin
      if MessageDlg('', 'Flag images archive is needed to display country flags.' + LineEnding + 'Download this archive now?', mtConfirmation, mbYesNo, 0, mbYes) <> mrYes then
        exit;
      if not DownloadFile(FlagsURL, FHomeDir) then
        exit;
    end;
  acShowCountryFlag.Checked:=not acShowCountryFlag.Checked;
  RpcObj.RefreshNow:=True;
end;

procedure TMainForm.acStartAllTorrentsExecute(Sender: TObject);
begin
  TorrentAction(0, 'start');
end;

procedure TMainForm.acStartTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'start');
end;

procedure TMainForm.acStopAllTorrentsExecute(Sender: TObject);
begin
  TorrentAction(0, 'stop');
end;

procedure TMainForm.acStopTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'stop');
end;

procedure TMainForm.acTorrentPropsExecute(Sender: TObject);
var
  req, args, t: TJSONObject;
  i, j, id: integer;
begin
  AppBusy;
  id:=RpcObj.CurTorrentId;
  with TTorrPropsForm.Create(Self) do
  try
    args:=RpcObj.RequestInfo(id, ['downloadLimit', 'downloadLimitMode',
                                  'uploadLimit', 'uploadLimitMode', 'name', 'maxConnectedPeers']);
    if args = nil then begin
      CheckStatus(False);
      exit;
    end;
    try
      t:=args.Arrays['torrents'].Objects[0];

      txName.Caption:=txName.Caption + ' ' + UTF8Encode(t.Strings['name']);

      j:=t.Integers['downloadLimitMode'];
      cbMaxDown.Checked:=j = TR_SPEEDLIMIT_SINGLE;
      i:=t.Integers['downloadLimit'];
      if (i < 0) or (j = TR_SPEEDLIMIT_UNLIMITED) then
        edMaxDown.ValueEmpty:=True
      else
        edMaxDown.Value:=i;

      j:=t.Integers['uploadLimitMode'];
      cbMaxUp.Checked:=j = TR_SPEEDLIMIT_SINGLE;
      i:=t.Integers['uploadLimit'];
      if (i < 0) or (j = TR_SPEEDLIMIT_UNLIMITED) then
        edMaxUp.ValueEmpty:=True
      else
        edMaxUp.Value:=i;
      edPeerLimit.Value:=t.Integers['maxConnectedPeers'];
    finally
      args.Free;
    end;
    cbMaxDownClick(nil);
    cbMaxUpClick(nil);
    AppNormal;
    if ShowModal = mrOk then begin
      AppBusy;
      Self.Update;
      req:=TJSONObject.Create;
      try
        req.Add('method', 'torrent-set');
        args:=TJSONObject.Create;
        args.Add('ids', TJSONArray.Create([id]));
        args.Add('speed-limit-down-enabled', TJSONIntegerNumber.Create(integer(cbMaxDown.Checked) and 1));
        if cbMaxDown.Checked then
          args.Add('speed-limit-down', TJSONIntegerNumber.Create(edMaxDown.Value));
        args.Add('speed-limit-up-enabled', TJSONIntegerNumber.Create(integer(cbMaxUp.Checked) and 1));
        if cbMaxUp.Checked then
          args.Add('speed-limit-up', TJSONIntegerNumber.Create(edMaxUp.Value));
        args.Add('peer-limit', TJSONIntegerNumber.Create(edPeerLimit.Value));
        req.Add('arguments', args);
        args:=nil;
        args:=RpcObj.SendRequest(req, False);
        if args = nil then begin
          CheckStatus(False);
          exit;
        end;
        args.Free;
      finally
        req.Free;
      end;
      RpcObj.RefreshNow:=True;
      AppNormal;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.acUpdateGeoIPExecute(Sender: TObject);
begin
  if DownloadGeoIpDatabase(True) then
    MessageDlg('Update complete.', mtInformation, [mbOK], 0);
end;

procedure TMainForm.acVerifyTorrentExecute(Sender: TObject);
begin
  if lvTorrents.Selected = nil then exit;
  if MessageDlg('', Format('Torrent verification may take a long time.'#13'Are you sure to start verification of torrent ''%s''?', [lvTorrents.Selected.Caption]), mtConfirmation, mbYesNo, 0, mbNo) <> mrYes then exit;
  TorrentAction(PtrUInt(lvTorrents.Selected.Data), 'verify');
end;

procedure TMainForm.ApplicationPropertiesException(Sender: TObject; E: Exception);
begin
  ForceAppNormal;
  MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateUI;
  Done:=True;
end;

procedure TMainForm.ApplicationPropertiesMinimize(Sender: TObject);
begin
{$ifdef windows}
  if FIni.ReadBool('Interface', 'TrayMinimize', True) then
    HideApp;
{$endif windows}
  UpdateTray;
end;

procedure TMainForm.ApplicationPropertiesRestore(Sender: TObject);
begin
  UpdateTray;
end;

procedure TMainForm.DummyTimerTimer(Sender: TObject);
var
  s: string;
begin
  DummyTimer.Enabled:=False;
  try
    if not FStarted then begin
      Application.ProcessMessages;
      FStarted:=True;
      acConnect.Execute;
      Application.ProcessMessages;
      panTransfer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
      panGeneralInfo.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
      panTransfer.ChildSizing.Layout:=cclNone;
      panGeneralInfo.ChildSizing.Layout:=cclNone;
      with panTransfer do
        ClientHeight:=Controls[ControlCount - 1].BoundsRect.Bottom + ChildSizing.TopBottomSpacing;
      with panGeneralInfo do
        ClientHeight:=Controls[ControlCount - 1].BoundsRect.Bottom + ChildSizing.TopBottomSpacing;

      if FIni.ReadBool('MainForm', 'FirstRun', True) then begin
        if not acResolveCountry.Checked then
          acResolveCountry.Execute;
        if acResolveCountry.Checked and not acShowCountryFlag.Checked then
          acShowCountryFlag.Execute;
        FIni.WriteBool('MainForm', 'FirstRun', False);
      end;
    end;

    if FileExists(FIPCFileName) then begin
      s:=ReadFileToString(FIPCFileName);
      DeleteFile(FIPCFileName);
      ShowApp;

      if s = '' then
        exit;

      if FileExistsUTF8(s) then
        DoAddTorrent(s);
    end;
  finally
    DummyTimer.Enabled:=True;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FIni.ReadBool('Interface', 'TrayClose', False) then begin
    CloseAction:=caHide;
    HideApp;
    UpdateTray;
    exit;
  end;
  BeforeCloseApp;
end;

procedure TMainForm.miToggleAppClick(Sender: TObject);
begin
  if miToggleApp.Caption = SHideApp then
    HideApp
  else
    ShowApp;
  UpdateTray;
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

procedure TMainForm.RefreshNowTimerTimer(Sender: TObject);
begin
  RefreshNowTimer.Enabled:=False;
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

procedure TMainForm.sbGenInfoResize(Sender: TObject);
begin
  sbGenInfo.HorzScrollBar.Visible:=False;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  miToggleApp.Click;
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
  TrayIcon.Hint:=RpcObj.InfoStatus;
  RpcObj.Connect;
end;

procedure TMainForm.DoDisconnect;

begin
  RefreshNowTimer.Enabled:=False;
  ClearDetailsInfo;
  lvTorrents.Clear;
  lvTorrents.Enabled:=False;
  lvTorrents.Color:=Self.Color;
  lvPeers.Enabled:=False;
  lvPeers.Color:=Self.Color;
  lvFiles.Enabled:=False;
  lvFiles.Color:=Self.Color;

  lvFilter.Enabled:=False;
  lvFilter.Color:=Self.Color;
  lvFilter.Items[0].Caption:=SAll;
  lvFilter.Items[1].Caption:=SDownloading;
  lvFilter.Items[2].Caption:=SCompleted;
  lvFilter.Items[3].Caption:=SActive;
  lvFilter.Items[4].Caption:=SInactive;

  RpcObj.Disconnect;

  RpcObj.InfoStatus:='Disconnected';
  CheckStatus;
  UpdateUI;
  TrayIcon.Hint:=RpcObj.InfoStatus;
  FTorrents.RowCnt:=0;
  lvFilter.Items[0].Selected:=True;
  while lvFilter.Items.Count > StatusFiltersCount do
    lvFilter.Items.Delete(lvFilter.Items.Count - 1);
  RefreshNowTimer.Enabled:=False;
end;

procedure TMainForm.ClearDetailsInfo;

  procedure ClearChildren(AParent: TPanel);
  var
    i: integer;
  begin
    AParent.AutoSize:=False;
    AParent.ChildSizing.Layout:=cclNone;
    for i:=0 to AParent.ControlCount - 1 do begin
      if AParent.Controls[i] is TLabel then
        with AParent.Controls[i] as TLabel do begin
          if (Length(Name) < 5) or (Copy(Name, Length(Name) - 4, 5) <> 'Label') then
            Caption:='';
          PopupMenu:=pmLabels;
        end;
    end;
  end;

begin
  lvPeers.Clear;
  lvFiles.Clear;
  ClearChildren(panGeneralInfo);
  ClearChildren(panTransfer);
  pbDownloaded.Position:=0;
  txDownProgress.AutoSize:=False;
  txDownProgress.Caption:='';
end;

procedure TMainForm.UpdateUI;
begin
  acAddTorrent.Enabled:=RpcObj.Connected;
  acDaemonOptions.Enabled:=RpcObj.Connected;
  acStartAllTorrents.Enabled:=RpcObj.Connected and (lvTorrents.Items.Count > 0);
  acStopAllTorrents.Enabled:=acStartAllTorrents.Enabled;
  acStartTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acStopTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acVerifyTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acRemoveTorrent.Enabled:=RpcObj.Connected and Assigned(lvTorrents.Selected);
  acTorrentProps.Enabled:=acRemoveTorrent.Enabled;

  acSetHighPriority.Enabled:=RpcObj.Connected and (lvTorrents.Selected <> nil) and
                      (lvFiles.Selected <> nil) and (PageInfo.ActivePage = tabFiles);
  acSetNormalPriority.Enabled:=acSetHighPriority.Enabled;
  acSetLowPriority.Enabled:=acSetHighPriority.Enabled;
  acSetNotDownload.Enabled:=acSetHighPriority.Enabled;
  acSetupColumns.Enabled:=RpcObj.Connected;
end;

function TMainForm.ShowConnOptions: boolean;
begin
  Result:=False;
  with TOptionsForm.Create(Self) do
  try
    edHost.Text:=FIni.ReadString('Connection', 'Host', 'localhost');
    edPort.Value:=FIni.ReadInteger('Connection', 'Port', 9091);
    edUserName.Text:=FIni.ReadString('Connection', 'UserName', '');
    if FIni.ReadString('Connection', 'Password', '') <> '' then
      edPassword.Text:='******';
    edRefreshInterval.Value:=FIni.ReadInteger('Connection', 'RefreshInterval', 5);

    cbTrayClose.Checked:=FIni.ReadBool('Interface', 'TrayClose', False);
{$ifdef windows}
    cbTrayMinimize.Checked:=FIni.ReadBool('Interface', 'TrayMinimize', True);
{$else}
    cbTrayMinimize.Enabled:=False;
{$endif}
    cbTrayIconAlways.Checked:=FIni.ReadBool('Interface', 'TrayIconAlways', True);

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

      FIni.WriteBool('Interface', 'TrayClose', cbTrayClose.Checked);
{$ifdef windows}
      FIni.WriteBool('Interface', 'TrayMinimize', cbTrayMinimize.Checked);
{$endif}
      FIni.WriteBool('Interface', 'TrayIconAlways', cbTrayIconAlways.Checked);

      if not RpcObj.Connected then
        DoConnect;

      UpdateTray;
      Result:=True;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.SaveColumns(LV: TListView; const AName: string; FullInfo: boolean);
var
  i, j: integer;
begin
  for i:=0 to LV.Columns.Count - 1 do
    with LV.Columns[i] do begin
      FIni.WriteInteger(AName, Format('Id%d', [i]), ID);
      if Visible or (Tag = 0) then
        j:=Width
      else
        j:=Tag;
      FIni.WriteInteger(AName, Format('Width%d', [i]), j);
      if FullInfo then begin
        FIni.WriteInteger(AName, Format('Index%d', [i]), Index);
        FIni.WriteBool(AName, Format('Visible%d', [i]), Visible);
      end;
    end;
end;

procedure TMainForm.LoadColumns(LV: TListView; const AName: string; FullInfo: boolean);
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
            if FullInfo then begin
              Index:=FIni.ReadInteger(AName, Format('Index%d', [i]), Index);
              Visible:=FIni.ReadBool(AName, Format('Visible%d', [i]), Visible);
            end;
            Tag:=FIni.ReadInteger(AName, Format('Width%d', [i]), Width);
            Width:=Tag;
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

function TMainForm.SecondsToString(j: integer): string;
begin
  if j < 60 then
    Result:=Format('%ds', [j])
  else
  if j < 60*60 then
    Result:=Format('%dm, %ds', [j div 60, j mod 60])
  else begin
    j:=(j + 30) div 60;
    if j < 60*24 then
      Result:=Format('%dh, %dm', [j div 60, j mod 60])
    else begin
      j:=(j + 30) div 60;
      Result:=Format('%dd, %dh', [j div 24, j mod 24])
    end;
  end;

end;

procedure TMainForm.FillTorrentsList(list: TJSONArray);
var
  i, j, row, id, StateImg: integer;
  t, tracker: TJSONObject;
  trackers: TJSONArray;
  f: double;
  ExistingRow: boolean;
  s: string;

  function GetTorrentValue(AIndex: integer; const AName: string; AType: integer): boolean;
  var
    res: variant;
  begin
    Result:=t.IndexOfName(AName) >= 0;
    if Result then
      case AType of
        vtInteger:
          res:=t.Integers[AName];
        vtExtended:
          res:=t.Floats[AName];
        else
          res:=t.Strings[AName];
      end
    else
      res:=NULL;

    FTorrents[AIndex, row]:=res;
  end;

begin
  if list = nil then begin
    ClearDetailsInfo;
    exit;
  end;

  for i:=0 to FTorrents.Count - 1 do
    FTorrents[idxTag, i]:=0;

  for i:=0 to list.Count - 1 do begin
    StateImg:=-1;

    t:=list[i] as TJSONObject;
    id:=t.Integers['id'];
    row:=FTorrents.Count;
    ExistingRow:=False;
    for j:=0 to FTorrents.Count - 1 do
      if FTorrents[idxTorrentId, j] = id then begin
        row:=j;
        ExistingRow:=True;
        break;
      end;

    FTorrents[idxTorrentId, row]:=t.Integers['id'];

    if t.IndexOfName('name') >= 0 then
      FTorrents[idxName, row]:=UTF8Encode(t.Strings['name']);

    j:=t.Integers['status'];
    if ExistingRow and (j = TR_STATUS_SEED) and (FTorrents[idxStatus, row] = TR_STATUS_DOWNLOAD) then
      DownloadFinished(FTorrents[idxName, row]);
    FTorrents[idxStatus, row]:=j;
    case j of
      TR_STATUS_CHECK_WAIT: StateImg:=imgDownQueue;
      TR_STATUS_CHECK:      StateImg:=imgDownQueue;
      TR_STATUS_DOWNLOAD:   StateImg:=imgDown;
      TR_STATUS_SEED:       StateImg:=imgSeed;
      TR_STATUS_STOPPED:    StateImg:=imgDone;
    end;

    FTorrents[idxTrackerStatus, row]:=t.Strings['announceResponse'];
    if (FTorrents[idxStatus, row] <> TR_STATUS_STOPPED) and (GetTorrentError(t) <> '') then
      if t.Strings['errorString'] <> '' then
        StateImg:=imgError
      else
        if StateImg in [imgDown,imgSeed] then
          Inc(StateImg, 2);

    if FTorrents[idxStatus, row] = TR_STATUS_CHECK then
      f:=t.Floats['recheckProgress']*100.0
    else begin
      f:=t.Floats['sizeWhenDone'];
      if f <> 0 then
        f:=(f - t.Floats['leftUntilDone'])*100.0/f;
      if (StateImg = imgDone) and (t.Floats['leftUntilDone'] <> 0) then
        StateImg:=imgStopped;
    end;
    FTorrents[idxDone, row]:=f;
    FTorrents[idxStateImg, row]:=StateImg;
    GetTorrentValue(idxDownSpeed, 'rateDownload', vtInteger);
    GetTorrentValue(idxUpSpeed, 'rateUpload', vtInteger);

    GetTorrentValue(idxSize, 'totalSize', vtExtended);
    GetTorrentValue(idxSeeds, 'peersSendingToUs', vtInteger);
    GetTorrentValue(idxSeedsTotal, 'seeders', vtInteger);
    GetTorrentValue(idxPeers, 'peersGettingFromUs', vtInteger);
    GetTorrentValue(idxLeechers, 'leechers', vtInteger);
    GetTorrentValue(idxPeersTotal, 'peersKnown', vtInteger);
    GetTorrentValue(idxETA, 'eta', vtInteger);
    GetTorrentValue(idxDownloaded, 'downloadedEver', vtExtended);
    GetTorrentValue(idxUploaded, 'uploadedEver', vtExtended);
    GetTorrentValue(idxAddedOn, 'addedDate', vtExtended);
    GetTorrentValue(idxCompletedOn, 'doneDate', vtExtended);
    GetTorrentValue(idxLastActive, 'activityDate', vtExtended);

    if t.IndexOfName('uploadRatio') >= 0 then begin
      f:=t.Floats['uploadRatio'];
      if f = -2 then
        f:=MaxInt;
      FTorrents[idxRatio, row]:=f;
    end
    else
      FTorrents[idxRatio, row]:=NULL;

    trackers:=t.Arrays['trackers'];
    if trackers <> nil then begin
      tracker:=trackers.Objects[0];
      if tracker <> nil then begin
        s:=UTF8Encode(tracker.Strings['announce']);
        j:=Pos('://', s);
        if j > 0 then
          s:=Copy(s, j + 3, MaxInt);
        j:=Pos('/', s);
        if j > 0 then
          s:=Copy(s, 1, j - 1);
        FTorrents[idxTracker, row]:=s;
      end
      else
        FTorrents[idxTracker, row]:='none';
    end
    else
      if VarIsEmpty(FTorrents[idxTracker, row]) then
        RpcObj.RequestFullInfo:=True;

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

  procedure SetSubItem(AID: integer; const s: string);
  var
    idx: integer;
  begin
    for idx:=0 to lvTorrents.Columns.Count - 1 do
      with lvTorrents.Columns[idx] do
        if ID = AID then begin
          if not Visible or (Width = 0) then
            break;
          if idx = 0 then
            it.Caption:=s
          else
            if it.SubItems.Count < idx then begin
              while it.SubItems.Count < idx - 1 do
                it.SubItems.Add('');
              it.SubItems.Add(s);
            end
            else
              it.SubItems[idx-1]:=s;
          break;
        end;
  end;

var
  i, j, FilterIdx: integer;
  s, TrackerFilter: string;
  UpSpeed, DownSpeed: double;
  Cnt, DownCnt, SeedCnt, CompletedCnt, ActiveCnt: integer;
  IsActive: boolean;
begin
//  lvTorrents.BeginUpdate;
  lvTorrents.Tag:=1;
  try
    lvTorrents.Enabled:=True;
    lvTorrents.Color:=clWindow;
    lvFilter.Enabled:=True;
    lvFilter.Color:=clWindow;

    FTorrents.Sort(FTorrentsSortColumn, FTorrentsSortDesc);

    for i:=0 to FTrackers.Count - 1 do
      FTrackers.Objects[i]:=nil;

    UpSpeed:=0;
    DownSpeed:=0;
    DownCnt:=0;
    SeedCnt:=0;
    CompletedCnt:=0;
    ActiveCnt:=0;
    Cnt:=0;

    if lvFilter.Selected <> nil then begin
      FilterIdx:=lvFilter.Selected.Index;
      if FilterIdx >= StatusFiltersCount then begin
        FilterIdx:=fltAll;
        TrackerFilter:=lvFilter.Selected.Caption;
        i:=Pos('(', TrackerFilter);
        if i > 0 then
          TrackerFilter:=Trim(Copy(TrackerFilter, 1, i - 1));
      end;
    end
    else
      FilterIdx:=fltAll;

    for i:=0 to FTorrents.Count - 1 do begin
      IsActive:=(FTorrents[idxDownSpeed, i] <> 0) or (FTorrents[idxUpSpeed, i] <> 0);
      if IsActive then
        Inc(ActiveCnt);

      case integer(FTorrents[idxStatus, i]) of
        TR_STATUS_DOWNLOAD:
          Inc(DownCnt);
        TR_STATUS_SEED:
          begin
            Inc(SeedCnt);
            Inc(CompletedCnt);
          end;
        TR_STATUS_STOPPED:
          if FTorrents[idxStateImg, i] = imgDone then
            s:='Finished';
      end;

      if not VarIsEmpty(FTorrents[idxTracker, i]) then begin
        s:=FTorrents[idxTracker, i];
        j:=FTrackers.IndexOf(s);
        if j < 0 then
          j:=FTrackers.Add(s);
        FTrackers.Objects[j]:=TObject(ptruint(FTrackers.Objects[j]) + 1);
        if (TrackerFilter <> '') and (TrackerFilter <> s) then
          continue;
      end;

      case FilterIdx of
        fltActive:
          if not IsActive then
            continue;
        fltInactive:
          if IsActive then
            continue;
        fltDown:
          if FTorrents[idxStatus, i] <> TR_STATUS_DOWNLOAD then
            continue;
        fltDone:
          if (FTorrents[idxStateImg, i] <> imgDone) and (FTorrents[idxStatus, i] <> TR_STATUS_SEED) then
            continue;
      end;

      if Cnt >= lvTorrents.Items.Count then
        it:=lvTorrents.Items.Add
      else
        it:=lvTorrents.Items[Cnt];

      SetSubItem(idxName, FTorrents[idxName, i]);
      if not VarIsNull(FTorrents[idxSize, i]) then
        SetSubItem(idxSize, GetHumanSize(FTorrents[idxSize, i], 0));

      if not VarIsNull(FTorrents[idxStatus, i]) then
        SetSubItem(idxStatus, GetTorrentStatus(i));

      SetSubItem(idxDone, Format('%.1f%%', [double(FTorrents[idxDone, i])]));

      if not VarIsNull(FTorrents[idxSeedsTotal, i]) then
        SetSubItem(idxSeeds, GetSeedsText(FTorrents[idxSeeds, i], FTorrents[idxSeedsTotal, i]));

      if not VarIsNull(FTorrents[idxPeers, i]) then
        SetSubItem(idxPeers, GetPeersText(FTorrents[idxPeers, i], FTorrents[idxPeersTotal, i], FTorrents[idxLeechers, i]));

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

      if not VarIsNull(FTorrents[idxETA, i]) then
        SetSubItem(idxETA, EtaToString(FTorrents[idxETA, i]));

      if not VarIsNull(FTorrents[idxRatio, i]) then
        SetSubItem(idxRatio, RatioToString(FTorrents[idxRatio, i]));

      if not VarIsNull(FTorrents[idxDownloaded, i]) then
        SetSubItem(idxDownloaded, GetHumanSize(FTorrents[idxDownloaded, i]));

      if not VarIsNull(FTorrents[idxUploaded, i]) then
        SetSubItem(idxUploaded, GetHumanSize(FTorrents[idxUploaded, i]));

      if not VarIsEmpty(FTorrents[idxTracker, i]) then
        SetSubItem(idxTracker, FTorrents[idxTracker, i]);
      SetSubItem(idxTrackerStatus, FTorrents[idxTrackerStatus, i]);

      if not VarIsNull(FTorrents[idxAddedOn, i]) then
        SetSubItem(idxAddedOn, TorrentDateTimeToString(FTorrents[idxAddedOn, i]));

      if not VarIsNull(FTorrents[idxCompletedOn, i]) then
        SetSubItem(idxCompletedOn, TorrentDateTimeToString(FTorrents[idxCompletedOn, i]));

      if not VarIsNull(FTorrents[idxLastActive, i]) then
        SetSubItem(idxLastActive, TorrentDateTimeToString(FTorrents[idxLastActive, i]));

      j:=FTorrents[idxTorrentId, i];
      if cardinal(j) = RpcObj.CurTorrentId then begin
        it.Focused:=True;
        it.Selected:=True;
      end;

      DownSpeed:=DownSpeed + FTorrents[idxDownSpeed, i];
      UpSpeed:=UpSpeed + FTorrents[idxUpSpeed, i];

      it.ImageIndex:=FTorrents[idxStateImg, i];
      it.Data:=pointer(ptruint(j));
      Inc(Cnt);
    end;

    while lvTorrents.Items.Count > Cnt do
      lvTorrents.Items.Delete(lvTorrents.Items.Count - 1);
  finally
    lvTorrents.Tag:=0;
//    lvTorrents.EndUpdate;
  end;

  lvFilter.Tag:=1;
  try
    lvFilter.Items[0].Caption:=Format('%s (%d)', [SAll, FTorrents.Count]);
    lvFilter.Items[1].Caption:=Format('%s (%d)', [SDownloading, DownCnt]);
    lvFilter.Items[2].Caption:=Format('%s (%d)', [SCompleted, CompletedCnt]);
    lvFilter.Items[3].Caption:=Format('%s (%d)', [SActive, ActiveCnt]);
    lvFilter.Items[4].Caption:=Format('%s (%d)', [SInactive, FTorrents.Count - ActiveCnt]);

    if StatusFiltersCount >= lvFilter.Items.Count then
      it:=lvFilter.Items.Add
    else
      it:=lvFilter.Items[StatusFiltersCount];
    it.Data:=pointer(ptrint(-1));

    i:=0;
    while i < FTrackers.Count do begin
      j:=ptruint(FTrackers.Objects[i]);
      if j > 0 then begin
        if i + StatusFiltersCount + 1 >= lvFilter.Items.Count then begin
          it:=lvFilter.Items.Add;
          it.ImageIndex:=5;
        end
        else
          it:=lvFilter.Items[i + StatusFiltersCount + 1];
        s:=FTrackers[i];
        it.Caption:=Format('%s (%d)', [s, j]);
        if s = TrackerFilter then
          it.Selected:=True;
        Inc(i);
      end
      else
        FTrackers.Delete(i);
    end;
    while lvFilter.Items.Count > i + StatusFiltersCount + 1 do
      lvFilter.Items.Delete(lvFilter.Items.Count - 1);
  finally
    lvFilter.Tag:=0;
  end;
  if lvFilter.Selected = nil then
    lvFilter.Items[0].Selected:=True;

  CheckStatus;

  StatusBar.Panels[1].Text:=Format('D: %s/s', [GetHumanSize(DownSpeed, 1)]);
  StatusBar.Panels[2].Text:=Format('U: %s/s', [GetHumanSize(UpSpeed, 1)]);

  TrayIcon.Hint:=Format('%s%s%d downloading, %d seeding%s%s, %s',
        [RpcObj.InfoStatus, LineEnding, DownCnt, SeedCnt, LineEnding, StatusBar.Panels[1].Text, StatusBar.Panels[2].Text]);

  if (RpcObj.CurTorrentId <> 0) and (lvTorrents.Selected = nil) then begin
    RpcObj.CurTorrentId:=0;
    ClearDetailsInfo;
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
  s, ip: string;
  hostinfo: PHostEntry;
  opt: TResolverOptions;
begin
  if list = nil then begin
    ClearDetailsInfo;
    exit;
  end;
//  lvPeers.BeginUpdate;
  try
    lvPeers.Enabled:=True;
    lvPeers.Color:=clWindow;
    if list = nil then begin
      lvPeers.Clear;
      exit;
    end;

    if FResolver = nil then begin
      opt:=[];
      if acResolveHost.Checked then
        Include(opt, roResolveIP);
      if acResolveCountry.Checked then
        Include(opt, roResolveCountry);
      if opt <> [] then
        FResolver:=TIpResolver.Create(GetGeoIpDatabase, opt);
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
      ip:=p.Strings['address'];
      if FResolver <> nil then
        hostinfo:=FResolver.Resolve(ip)
      else
        hostinfo:=nil;
      if hostinfo <> nil then
        s:=hostinfo^.HostName
      else
        s:=ip;
      port:=p.Integers['port'];
      it:=nil;
      for j:=0 to High(ports) do
        if (port = ptruint(ports[j])) and ((lvPeers.Items[j].Caption = s) or (lvPeers.Items[j].Caption = ip)) then begin
          it:=lvPeers.Items[j];
          break;
        end;
      if it = nil then
        it:=lvPeers.Items.Add;

      it.Caption:=s;

      if hostinfo <> nil then
        s:=UTF8Encode(hostinfo^.CountryName)
      else
        s:='';

      SetSubItem(idxPeerCountry, s);
      if acShowCountryFlag.Checked and (hostinfo <> nil) then begin
        if hostinfo^.ImageIndex = 0 then begin
          hostinfo^.ImageIndex:=GetFlagImage(hostinfo^.CountryCode);
{$ifdef LCLgtk2}
          if hostinfo^.ImageIndex <> 0 then begin
            lvPeers.SmallImages:=nil;
            lvPeers.SmallImages:=imgFlags;
          end;
{$endif}
        end;
        j:=hostinfo^.ImageIndex
      end
      else
        j:=0;
      it.ImageIndex:=j;

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
  if (list = nil) or (priorities = nil) or (wanted = nil) then begin
    ClearDetailsInfo;
    exit;
  end;
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
  i, j, idx: integer;
  it: TListItem;
  s: string;
  f: double;
begin
  if (lvTorrents.Selected = nil) or (t = nil) then begin
    ClearDetailsInfo;
    exit;
  end;
  it:=lvTorrents.Selected;
  idx:=-1;
  for i:=0 to FTorrents.Count - 1 do
    if FTorrents[idxTorrentId, i] = PtrUInt(it.Data) then begin
      idx:=i;
      break;
    end;

  if idx = -1 then begin
    ClearDetailsInfo;
    exit;
  end;

  pbDownloaded.Position:=Round(FTorrents[idxDone, idx]*10);
  txDownProgress.Caption:=Format('%.1f%%', [double(FTorrents[idxDone, idx])]);
  txDownProgress.AutoSize:=True;

  panTransfer.ChildSizing.Layout:=cclNone;
  txStatus.Caption:=GetTorrentStatus(idx);
  txError.Caption:=GetTorrentError(t);
  txRemaining.Caption:=EtaToString(t.Integers['eta']);
  txDownloaded.Caption:=GetHumanSize(t.Floats['downloadedEver']);
  txUploaded.Caption:=GetHumanSize(t.Floats['uploadedEver']);
  txWasted.Caption:=Format('%s (%d hashfails)', [GetHumanSize(t.Floats['corruptEver']), Round(t.Floats['corruptEver']/t.Floats['pieceSize'])]);
  txDownSpeed.Caption:=GetHumanSize(FTorrents[idxDownSpeed, idx], 1)+'/s';
  txUpSpeed.Caption:=GetHumanSize(FTorrents[idxUpSpeed, idx], 1)+'/s';
  txRatio.Caption:=RatioToString(t.Floats['uploadRatio']);

  j:=t.Integers['downloadLimitMode'];
  if j = TR_SPEEDLIMIT_GLOBAL then
    s:='-'
  else begin
    i:=t.Integers['downloadLimit'];
    if (i < 0) or (j = TR_SPEEDLIMIT_UNLIMITED) then
      s:=Utf8Encode(WideChar($221E))
    else
      s:=GetHumanSize(i*1024)+'/s';
  end;
  txDownLimit.Caption:=s;

  j:=t.Integers['uploadLimitMode'];
  if j = TR_SPEEDLIMIT_GLOBAL then
    s:='-'
  else begin
    i:=t.Integers['uploadLimit'];
    if (i < 0) or (j = TR_SPEEDLIMIT_UNLIMITED) then
      s:=Utf8Encode(WideChar($221E))
    else
      s:=GetHumanSize(i*1024)+'/s';
  end;
  txUpLimit.Caption:=s;

  f:=t.Floats['nextAnnounceTime'];
  if f = 0 then
    s:='-'
  else
  if f = 1 then
    s:='Updating...'
  else
    s:=DateTimeToStr(UnixToDateTime(Trunc(f)) + GetTimeZoneDelta);
  txTrackerUpdate.Caption:=s;
  s:=GetSeedsText(t.Integers['peersSendingToUs'], t.Integers['seeders']);
  txSeeds.Caption:=StringReplace(s, '/', ' of ', []) + ' connected';
  s:=GetPeersText(t.Integers['peersGettingFromUs'], t.Integers['peersKnown'], t.Integers['leechers']);
  s:=StringReplace(s, ' ', ' connected ', []);
  s:=StringReplace(s, '/', ' of ', []);
  txPeers.Caption:=StringReplace(s, ')', ' in swarm)', []);
  txMaxPeers.Caption:=t.Strings['maxConnectedPeers'];
  panTransfer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;

  panGeneralInfo.ChildSizing.Layout:=cclNone;

  txTorrentName.Caption:=it.Caption;
  txCreated.Caption:=Format('%s by %s', [TorrentDateTimeToString(Trunc(t.Floats['dateCreated'])), t.Strings['creator']]);
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
  i: integer;
begin
  s:=RpcObj.Status;
  if s <> '' then begin
    RpcObj.Status:='';
    if Fatal then
      DoDisconnect;
    ForceAppNormal;
    MessageDlg(s, mtError, [mbOK], 0);
  end;
  if StatusBar.Panels[0].Text <> RpcObj.InfoStatus then begin
    StatusBar.Panels[0].Text:=RpcObj.InfoStatus;
    TrayIcon.Hint:=RpcObj.InfoStatus;
  end;
  if not RpcObj.Connected then
    for i:=1 to StatusBar.Panels.Count - 1 do
      StatusBar.Panels[i].Text:='';
end;

function TMainForm.TorrentAction(TorrentId: integer; const AAction: string): boolean;
var
  req, args: TJSONObject;
begin
  AppBusy;
  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-' + AAction);
    args:=TJSONObject.Create;
    if TorrentId <> 0 then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    req.Add('arguments', args);
    args:=RpcObj.SendRequest(req, False);
    Result:=args <> nil;
    args.Free;
  finally
    req.Free;
  end;
  if not Result then
    CheckStatus(False)
  else
    RpcObj.RefreshNow:=True;
  AppNormal;
end;

function TMainForm.SetFilePriority(TorrentId: integer; const Files: array of integer; const APriority: string): boolean;

  function CreateFilesArray: TJSONArray;
  var
    i: integer;
  begin
    Result:=TJSONArray.Create;
    for i:=Low(Files) to High(Files) do
      Result.Add(Files[i]);
  end;

var
  req, args: TJSONObject;
begin
  AppBusy;
  req:=TJSONObject.Create;
  try
    req.Add('method', 'torrent-set');
    args:=TJSONObject.Create;
    if TorrentId <> 0 then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    if APriority = 'skip' then
      args.Add('files-unwanted', CreateFilesArray)
    else begin
      args.Add('files-wanted', CreateFilesArray);
      args.Add('priority-' + APriority, CreateFilesArray);
    end;
    req.Add('arguments', args);
    args:=RpcObj.SendRequest(req, False);
    Result:=args<> nil;
    args.Free;
  finally
    req.Free;
  end;
  if not Result then
    CheckStatus(False)
  else
    RpcObj.RefreshNow:=True;
  AppNormal;
end;

function TMainForm.SetCurrentFilePriority(const APriority: string): boolean;
var
  Files: array of integer;
  i, j: integer;
begin
  if (lvTorrents.Selected = nil) or (PageInfo.ActivePage <> tabFiles) then exit;
  SetLength(Files, lvFiles.Items.Count);
  j:=0;
  for i:=0 to lvFiles.Items.Count - 1 do
    with lvFiles.Items[i] do
      if Selected then begin
        Files[j]:=PtrUInt(Data) - 1;
        Inc(j);
      end;
  if j = 0 then exit;
  SetLength(Files, j);
  Result:=SetFilePriority(PtrUInt(lvTorrents.Selected.Data), Files, APriority);
end;

initialization
  {$I main.lrs}

end.

{$I main.lrs}

end.

ata) - 1, APriority);
end;

initialization
  {$I main.lrs}

end.

{$I main.lrs}

end.

tion
  {$I main.lrs}

end.

{$I main.lrs}

end.

