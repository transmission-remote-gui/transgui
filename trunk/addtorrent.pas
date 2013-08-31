{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2012 by Yury Sidorov.

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

unit AddTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, VarGrid, Grids,
  ButtonPanel, ExtCtrls, BaseForm, varlist, fpjson;

resourcestring
  SSize = 'Size';
  SSelectDownloadFolder = 'Select a folder for download';
  SInvalidName = 'Invalid name specified.';

type
  TFilesTree = class;

  { TAddTorrentForm }

  TAddTorrentForm = class(TBaseForm)
    btSelectAll: TButton;
    btSelectNone: TButton;
    btBrowse: TButton;
    Buttons: TButtonPanel;
    cbStartTorrent: TCheckBox;
    cbDestFolder: TComboBox;
    edSaveAs: TEdit;
    gbSaveAs: TGroupBox;
    gbContents: TGroupBox;
    edPeerLimit: TSpinEdit;
    DiskSpaceTimer: TTimer;
    txSaveAs: TLabel;
    txSize: TLabel;
    txDiskSpace: TLabel;
    txPeerLimit: TLabel;
    lvFiles: TVarGrid;
    txDestFolder: TLabel;
    procedure btBrowseClick(Sender: TObject);
    procedure btSelectAllClick(Sender: TObject);
    procedure btSelectNoneClick(Sender: TObject);
    procedure cbDestFolderChange(Sender: TObject);
    procedure DiskSpaceTimerTimer(Sender: TObject);
    procedure edSaveAsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FDiskSpaceCaption: string;
    FTree: TFilesTree;
    procedure TreeStateChanged(Sender: TObject);
    procedure UpdateSize;
  public
    OrigCaption: string;
    property FilesTree: TFilesTree read FTree;
  end;

  { TFilesTree }

  TFilesTree = class
  private
    FCheckboxes: boolean;
    FGrid: TVarGrid;
    FHasFolders: boolean;
    FIsPlain: boolean;
    FOnStateChange: TNotifyEvent;
    FFiles: TVarList;
    FTorrentId: integer;
    FLastFileCount: integer;

    procedure CollapseFolder(ARow: integer);
    procedure DoCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
    procedure DoCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
    procedure DoTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
    procedure DoAfterSort(Sender: TObject);
    procedure ExpandFolder(ARow: integer);
    function GetChecked(ARow: integer): TCheckBoxState;
    function GetExpanded(ARow: integer): boolean;
    procedure SetCheckboxes(const AValue: boolean);
    procedure IntSetChecked(ARow: integer; const AValue: TCheckBoxState);
    procedure SetChecked(ARow: integer; const AValue: TCheckBoxState);
    procedure SetExpanded(ARow: integer; const AValue: boolean);
    procedure SetIsPlain(const AValue: boolean);
    procedure TreeChanged;
    procedure DoOnStateChange;
    function DoCompareVarRows(Sender: TVarList; const Row1, Row2: variant; DescendingSort: boolean): integer;
    procedure SetRowOption(ARow, AOption: integer; DoSet: boolean);
  public
    constructor Create(AGrid: TVarGrid); reintroduce;
    function IsFolder(ARow: integer): boolean;
    procedure CollapseAll;
    procedure FillTree(ATorrentId: integer; files, priorities, wanted: TJSONArray);
    procedure SetStateAll(AState: TCheckBoxState);
    property Grid: TVarGrid read FGrid;
    property HasFolders: boolean read FHasFolders;
    property Checkboxes: boolean read FCheckboxes write SetCheckboxes;
    property IsPlain: boolean read FIsPlain write SetIsPlain;
    property Expanded[ARow: integer]: boolean read GetExpanded write SetExpanded;
    property Checked[ARow: integer]: TCheckBoxState read GetChecked write SetChecked;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

const
  // Files list columns
  idxFileName      = 0;
  idxFileSize      = 1;
  idxFileDone      = 2;
  idxFileProgress  = 3;
  idxFilePriority  = 4;
  idxFileId        = -1;
  idxFileTag       = -2;
  idxFileFullPath  = -3;
  idxFileLevel     = -4;
  idxFileIndex     = -5;

  FilesExtraColumns = 5;

implementation

uses lclintf, lcltype, main, variants, Utils, rpc;

const
  roChecked   = $30000;
  roCollapsed = $40000;
  roHidden    = $80000;

  roCheckedShift = 16;

{ TFilesTree }

constructor TFilesTree.Create(AGrid: TVarGrid);
begin
  FGrid:=AGrid;
  FFiles:=FGrid.Items;
  FGrid.OnCheckBoxClick:=@DoCheckBoxClick;
  FGrid.OnTreeButtonClick:=@DoTreeButtonClick;
  FGrid.OnCellAttributes:=@DoCellAttributes;
  FGrid.OnAfterSort:=@DoAfterSort;
end;

function TFilesTree.IsFolder(ARow: integer): boolean;
begin
  Result:=VarIsEmpty(FGrid.Items[idxFileId, ARow]);
end;

procedure TFilesTree.CollapseAll;
var
  i: integer;
begin
  AppBusy;
  FGrid.BeginUpdate;
  try
    for i:=0 to FGrid.Items.Count - 1 do begin
      if IsFolder(i) then
        SetRowOption(i, roCollapsed, True);
      if integer(FGrid.Items[idxFileLevel, i]) > 0 then begin
        FGrid.RowVisible[i]:=False;
        SetRowOption(i, roHidden, True);
      end;
    end;
    TreeChanged;
  finally
    FGrid.EndUpdate;
  end;
  AppNormal;
end;

procedure TFilesTree.FillTree(ATorrentId: integer; files, priorities, wanted: TJSONArray);

  function _AddFolders(list: TVarList; const path: string; var idx: integer; cnt, level: integer): double;
  var
    s, ss: string;
    j: integer;
    d: double;
    p: PChar;
  begin
    Result:=0;
    while idx < cnt do begin
      s:=ExtractFilePath(UTF8Encode(widestring(list[idxFileFullPath, idx])));
      if s = '' then begin
        Inc(idx);
        continue;
      end;
      if (path <> '') and (Pos(path, s) <> 1)  then
        break;
      if s = path then begin
        Result:=Result + list[idxFileSize, idx];
        list[idxFileLevel, idx]:=level;
        Inc(idx);
      end
      else begin
        ss:=Copy(s, Length(path) + 1, MaxInt);
        p:=PChar(ss);
        while (p^ <> #0) and not (p^ in ['/','\']) do
          Inc(p);
        if p^ <> #0 then begin
          SetLength(ss, p - PChar(ss) + 1);
          j:=list.Count;
          list[idxFileLevel, j]:=level;
          list[idxFileFullPath, j]:=UTF8Decode(path + ss);
          d:=_AddFolders(list, path + ss, idx, cnt, level + 1);
          list[idxFileSize, j]:=d;
          ss:=ExcludeTrailingPathDelimiter(ss);
          list[idxFileName, j]:=UTF8Decode(ExtractFileName(ss));
          Result:=Result + d;
        end;
      end;
    end;
  end;

var
  i, row: integer;
  FullRefresh: boolean;
  f: TJSONObject;
  s, ss, path: string;
  ff: double;
begin
  if files = nil then begin
    FGrid.Items.Clear;
    exit;
  end;
  FullRefresh:=(FTorrentId <> ATorrentId) or (FLastFileCount <> files.Count);
  FFiles.BeginUpdate;
  try
    FFiles.OnCompareVarRows:=nil;
    if FullRefresh then
      FFiles.Clear
    else begin
      for i:=0 to FFiles.Count - 1 do
        FFiles[idxFileTag, i]:=0;
      FFiles.Sort(idxFileId);
    end;

    // Detecting top level folder to be removed
    path:='';
    if files.Count > 0 then begin
      s:=UTF8Encode(files.Objects[0].Strings['name']);
      i:=Pos(RemotePathDelimiter, s);
      if i > 0 then
        path:=Copy(s, 1, i);
    end;

    for i:=0 to files.Count - 1 do begin
      f:=files.Objects[i];
      if FullRefresh then
        row:=i
      else
        if not FFiles.Find(idxFileId, i, row) then
          FFiles.InsertRow(row);
      FFiles[idxFileTag, row]:=1;
      FFiles[idxFileId, row]:=i;
      FFiles[idxFileLevel, row]:=0;

      s:=UTF8Encode(f.Strings['name']);
      FFiles[idxFileFullPath, row]:=UTF8Decode(ExtractFilePath(s));
      if (path <> '') and (Copy(s, 1, Length(path)) = path) then
        s:=Copy(s, Length(path) + 1, MaxInt);
      ss:=ExtractFileName(s);
      if ss <> s then
        FHasFolders:=True;
      FFiles[idxFileName, row]:=UTF8Decode(ss);
      ff:=f.Floats['length'];
      FFiles[idxFileSize, row]:=ff;

      if FGrid.Columns.Count > idxFileDone then begin
        FFiles[idxFileDone, row]:=f.Floats['bytesCompleted'];
        if ff = 0 then
          ff:=100.0
        else
          ff:=double(FFiles[idxFileDone, row])*100.0/ff;
        FFiles[idxFileProgress, row]:=Int(ff*10.0)/10.0;

        if (priorities <> nil) and (wanted <> nil) then begin
          if wanted.Integers[i] = 0 then
            FFiles[idxFilePriority, row]:=TR_PRI_SKIP
          else
            FFiles[idxFilePriority, row]:=priorities.Integers[i];
        end;
      end;
    end;

    if not FullRefresh then begin
      i:=0;
      while i < FFiles.Count do
        if FFiles[idxFileTag, i] = 0 then
          FFiles.Delete(i)
        else
          Inc(i);
    end;

    if HasFolders and FullRefresh then begin
      FFiles.Sort(idxFileFullPath);
      i:=0;
      _AddFolders(FFiles, path, i, FFiles.Count, 0);
    end;

    FFiles.OnCompareVarRows:=@DoCompareVarRows;
    FGrid.Sort;
    if FullRefresh and (FFiles.Count > 0) then begin
      FGrid.Row:=0;
      if HasFolders then begin
        i:=FFiles.RowCnt + FGrid.FixedRows;
        if FGrid.RowCount <> i then
          FGrid.RowCount:=i;
        CollapseAll;
      end;
    end;
  finally
    FFiles.EndUpdate;
  end;
end;

procedure TFilesTree.SetStateAll(AState: TCheckBoxState);
var
  i: integer;
begin
  FFiles.BeginUpdate;
  try
    for i:=0 to FFiles.Count - 1 do
      IntSetChecked(i, AState);
  finally
    FFiles.EndUpdate;
  end;
  DoOnStateChange;
end;

procedure TFilesTree.DoCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
begin
  if Checked[ARow] = cbChecked then
    Checked[ARow]:=cbUnchecked
  else
    Checked[ARow]:=cbChecked;
end;

procedure TFilesTree.DoTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
begin
  Expanded[ARow]:=not Expanded[ARow];
end;

procedure TFilesTree.DoAfterSort(Sender: TObject);
var
  p: boolean;
begin
  p:=FGrid.SortColumn <> idxFileName;
  if p <> IsPlain then
    IsPlain:=p
  else
    TreeChanged;
end;

procedure TFilesTree.CollapseFolder(ARow: integer);
var
  i, lev: integer;
begin
  AppBusy;
  FGrid.BeginUpdate;
  try
    lev:=FGrid.Items[idxFileLevel, ARow];
    SetRowOption(ARow, roCollapsed, True);
    for i:=ARow + 1 to FGrid.Items.Count - 1 do
      if integer(FGrid.Items[idxFileLevel, i]) > lev then begin
        FGrid.RowVisible[i]:=False;
        SetRowOption(i, roHidden, True);
      end
      else
        break;
    TreeChanged;
  finally
    FGrid.EndUpdate;
  end;
  AppNormal;
end;

procedure TFilesTree.ExpandFolder(ARow: integer);
var
  i, j, lev: integer;
begin
  AppBusy;
  FGrid.BeginUpdate;
  try
    lev:=FGrid.Items[idxFileLevel, ARow] + 1;
    SetRowOption(ARow, roCollapsed, False);
    for i:=ARow + 1 to FGrid.Items.Count - 1 do begin
      j:=integer(FGrid.Items[idxFileLevel, i]);
      if j = lev then begin
        FGrid.RowVisible[i]:=True;
        SetRowOption(i, roHidden, False);
        if IsFolder(i) and Expanded[i] then
          ExpandFolder(i);
      end
      else
        if j <= lev then
          break;
    end;
    TreeChanged;
  finally
    FGrid.EndUpdate;
  end;
  AppNormal;
end;

function TFilesTree.GetChecked(ARow: integer): TCheckBoxState;
begin
  Result:=TCheckBoxState((FFiles.RowOptions[ARow] and roChecked) shr roCheckedShift);
end;

function TFilesTree.GetExpanded(ARow: integer): boolean;
begin
  Result:=not LongBool(FFiles.RowOptions[ARow] and roCollapsed);
end;

procedure TFilesTree.SetCheckboxes(const AValue: boolean);
begin
  if FCheckboxes = AValue then exit;
  FCheckboxes:=AValue;
end;

procedure TFilesTree.IntSetChecked(ARow: integer; const AValue: TCheckBoxState);
begin
  FFiles.RowOptions[ARow]:=(FFiles.RowOptions[ARow] and not roChecked) or (integer(AValue) shl roCheckedShift);
end;

procedure TFilesTree.SetChecked(ARow: integer; const AValue: TCheckBoxState);
var
  i, lev: integer;
  st: TCheckBoxState;
begin
  st:=AValue;
  if st = cbGrayed then
    st:=cbUnchecked;
  if Checked[ARow] = st then
    exit;
  IntSetChecked(ARow, st);
  lev:=integer(FFiles[idxFileLevel, ARow]);

  if VarIsEmpty(FFiles[idxFileId, ARow]) then begin
    FFiles.BeginUpdate;
    for i:=ARow + 1 to FFiles.Count - 1 do
      if integer(FFiles[idxFileLevel, i]) <= lev then
        break
      else
        IntSetChecked(i, st);
    FFiles.EndUpdate;
  end;

  if lev > 0 then begin
    i:=ARow + 1;
    while (i < FFiles.Count) and (integer(FFiles[idxFileLevel, i]) >= lev) do
      Inc(i);

    for i:=i - 1 downto 0 do begin
      if VarIsEmpty(FFiles[idxFileId, i]) and (integer(FFiles[idxFileLevel, i]) < lev) then begin
        IntSetChecked(i, st);
        Dec(lev);
        if lev = 0 then
          break;
      end
      else
        if Checked[i] <> st then
          st:=cbGrayed;
    end;
  end;
  DoOnStateChange;
end;

procedure TFilesTree.SetExpanded(ARow: integer; const AValue: boolean);
begin
  if GetExpanded(ARow) <> AValue then
    if AValue then
      ExpandFolder(ARow)
    else
      CollapseFolder(ARow);
end;

procedure TFilesTree.SetIsPlain(const AValue: boolean);
begin
  if FIsPlain = AValue then exit;
  FIsPlain:=AValue;
  TreeChanged;
  if FFiles.Count > 0 then
    FGrid.Row:=0;
end;

procedure TFilesTree.TreeChanged;
var
  i, j: integer;
  f: boolean;
begin
  FGrid.Items.BeginUpdate;
  try
    FGrid.RowCount:=FFiles.RowCnt + FGrid.FixedRows;
    j:=0;
    for i:=0 to FGrid.Items.Count - 1 do begin
      if IsPlain then
        f:=not IsFolder(i)
      else
        f:=not LongBool(FFiles.RowOptions[i] and roHidden);
      FGrid.RowVisible[i]:=f;
      if f then begin
        FGrid.Items[idxFileIndex, i]:=j;
        Inc(j);
      end;
    end;
  finally
    FGrid.Items.EndUpdate;
  end;
end;

procedure TFilesTree.DoOnStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

function TFilesTree.DoCompareVarRows(Sender: TVarList; const Row1, Row2: variant; DescendingSort: boolean): integer;
begin
  if FGrid.SortColumn <> idxFileName then begin
    Result:=(integer(VarIsEmpty(Sender.GetRowItem(Row1, idxFileId))) and 1) - (integer(VarIsEmpty(Sender.GetRowItem(Row2, idxFileId))) and 1);
    exit;
  end;
  Result:=CompareVariants(Sender.GetRowItem(Row1, idxFileFullPath), Sender.GetRowItem(Row2, idxFileFullPath));
  if DescendingSort then
    Result:=-Result;
  if Result = 0 then
    Result:=(integer(VarIsEmpty(Sender.GetRowItem(Row2, idxFileId))) and 1) - (integer(VarIsEmpty(Sender.GetRowItem(Row1, idxFileId))) and 1);
end;

procedure TFilesTree.SetRowOption(ARow, AOption: integer; DoSet: boolean);
var
  i: integer;
begin
  i:=FFiles.RowOptions[ARow];
  if DoSet then
    FFiles.RowOptions[ARow]:=i or AOption
  else
    FFiles.RowOptions[ARow]:=i and not AOption;
end;

procedure TFilesTree.DoCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
begin
  if ARow < 0 then exit;
  with CellAttribs do begin
    if not (gdSelected in AState) and (integer(Sender.Items[idxFileIndex, ARow]) and 1 = 1) then
      Sender.Canvas.Brush.Color:=FAlterColor;
    if Text = '' then exit;
    case ADataCol of
      0:
        begin
//          Text:=UTF8Encode(Sender.Items[idxFileFullPath, ARow]);
          Indent:=integer(Sender.Items[idxFileLevel, ARow])*16;
          Options:=[coDrawCheckBox];
          State:=Checked[ARow];
          if IsFolder(ARow) then begin
            Include(Options, coDrawTreeButton);
            Expanded:=Self.Expanded[ARow];
            ImageIndex:=22;
          end
          else
            if HasFolders then
              Inc(Indent, Sender.RowHeights[ARow + Sender.FixedRows]);
        end;
      idxFileSize, idxFileDone:
        Text:=GetHumanSize(double(Sender.Items[ADataCol, ARow]));
      idxFileProgress:
        Text:=Format('%.1f%%', [double(Sender.Items[ADataCol, ARow])]);
      idxFilePriority:
        Text:=PriorityToStr(Sender.Items[idxFilePriority, ARow], ImageIndex);
    end;
  end;
end;

{ TAddTorrentForm }

procedure TAddTorrentForm.FormShow(Sender: TObject);
begin
  AppBusy;
  lvFiles.BeginUpdate;
  try
    btSelectAllClick(nil);
{
    lvFiles.Sort;
    if lvFiles.Items.Count > 0 then
      lvFiles.Row:=0;
}
//    FTree.CollapseAll;
  finally
    lvFiles.EndUpdate;
  end;
  DiskSpaceTimerTimer(nil);
  AppNormal;
end;

procedure TAddTorrentForm.OKButtonClick(Sender: TObject);
begin
  if edSaveAs.Enabled then begin
    edSaveAs.Text:=Trim(edSaveAs.Text);
    if edSaveAs.Text = '' then begin
      edSaveAs.SetFocus;
      MessageDlg(SInvalidName, mtError, [mbOK], 0);
      exit;
    end;
  end;
  ModalResult:=mrOK;
end;

procedure TAddTorrentForm.UpdateSize;
var
  i: integer;
  d, sz, tsz: double;
  s: string;
begin
  sz:=0;
  tsz:=0;
  for i:=0 to lvFiles.Items.Count - 1 do
    if not FTree.IsFolder(i) then begin
      d:=double(lvFiles.Items[idxFileSize, i]);
      tsz:=tsz + d;
      if FTree.Checked[i] = cbChecked then
        sz:=sz + d;
    end;

  s:=GetHumanSize(sz);
  if s <> GetHumanSize(tsz) then
    s:=s + ' / ' + GetHumanSize(tsz);
  txSize.Caption:=Format('%s: %s', [SSize, s]);
end;

procedure TAddTorrentForm.btSelectAllClick(Sender: TObject);
begin
  FTree.SetStateAll(cbChecked);
end;

procedure TAddTorrentForm.btBrowseClick(Sender: TObject);
var
  s: string;
begin
  s:=MainForm.SelectRemoteFolder(cbDestFolder.Text, SSelectDownloadFolder);
  if s <> '' then
    cbDestFolder.Text:=s;
end;

procedure TAddTorrentForm.btSelectNoneClick(Sender: TObject);
begin
  FTree.SetStateAll(cbUnchecked);
end;

procedure TAddTorrentForm.cbDestFolderChange(Sender: TObject);
begin
  DiskSpaceTimer.Enabled:=True;
end;

procedure TAddTorrentForm.DiskSpaceTimerTimer(Sender: TObject);
var
  f: double;
  req, args: TJSONObject;
begin
  DiskSpaceTimer.Enabled:=False;
  if RpcObj.RPCVersion < 15 then
    exit;
  AppBusy;
  f:=-1;
  try
    req:=TJSONObject.Create;
    args:=TJSONObject.Create;
    try
      req.Add('method', 'free-space');
      args.Add('path', UTF8Decode(cbDestFolder.Text));
      req.Add('arguments', args);
      args:=RpcObj.SendRequest(req);
      if args <> nil then
        f:=args.Floats['size-bytes'];
      RpcObj.Status:='';
    finally
      args.Free;
      req.Free;
    end;
  except
    f:=-1;
  end;
  txDiskSpace.Caption:=FDiskSpaceCaption + ' ' + GetHumanSize(f);
  AppNormal;
end;

procedure TAddTorrentForm.edSaveAsChange(Sender: TObject);
begin
  Caption:=OrigCaption + ' - ' + edSaveAs.Text;
end;

procedure TAddTorrentForm.TreeStateChanged(Sender: TObject);
begin
  UpdateSize;
end;

procedure TAddTorrentForm.FormCreate(Sender: TObject);
begin
  OrigCaption:=Caption;
  FDiskSpaceCaption:=txDiskSpace.Caption;
  lvFiles.Items.ExtraColumns:=FilesExtraColumns;
  FTree:=TFilesTree.Create(lvFiles);
  FTree.Checkboxes:=True;
  FTree.OnStateChange:=@TreeStateChanged;
  Buttons.OKButton.ModalResult:=mrNone;
{$ifdef windows}
  gbSaveAs.Caption:='';
{$endif windows}
{$ifdef darwin}
  Buttons.BorderSpacing.Right:=Buttons.BorderSpacing.Right + ScaleInt(12);
{$endif darwin}
end;

initialization
  {$I addtorrent.lrs}

end.

