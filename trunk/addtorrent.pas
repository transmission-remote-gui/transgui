{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2010 by Yury Sidorov.

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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, VarGrid, Grids, ButtonPanel;

resourcestring
  SSize = 'Size';

type

  { TAddTorrentForm }

  TAddTorrentForm = class(TForm)
    btSelectAll: TButton;
    btSelectNone: TButton;
    Buttons: TButtonPanel;
    cbStartTorrent: TCheckBox;
    cbDestFolder: TComboBox;
    gbSaveAs: TGroupBox;
    gbContents: TGroupBox;
    edPeerLimit: TSpinEdit;
    txSize: TLabel;
    txDiskSpace: TLabel;
    txPeerLimit: TLabel;
    lvFiles: TVarGrid;
    txDestFolder: TLabel;
    procedure btSelectAllClick(Sender: TObject);
    procedure btSelectNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
  private
    procedure lvFilesCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
    procedure lvFilesTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
    procedure CollapseFolder(ARow: integer);
    procedure ExpandFolder(ARow: integer);
    procedure TreeChanged;
    procedure UpdateSize;
  public
    HasFolders: boolean;
  end;

const
  idxAtName      = 0;
  idxAtSize      = 1;
  idxAtFullPath  = 2;
  idxAtFileID    = -1;
  idxAtLevel     = -2;
  idxAtChecked   = -3;
  idxAtCollapsed = -4;
  idxAtIndex     = -5;

implementation

uses lclintf, lcltype, main, variants;

{ TAddTorrentForm }

procedure TAddTorrentForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  btSelectAllClick(nil);
  lvFiles.Sort;
  if lvFiles.Items.Count > 0 then
    lvFiles.Row:=0;
  for i:=0 to lvFiles.Items.Count - 1 do
    if VarIsEmpty(lvFiles.Items[idxAtFileID, i]) then
      CollapseFolder(i);
  TreeChanged;
end;

procedure TAddTorrentForm.lvFilesCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
var
  i: integer;
begin
  if ARow < 0 then exit;
  with CellAttribs do begin
    if not (gdSelected in AState) and (integer(Sender.Items[idxAtIndex, ARow]) and 1 = 1) then
      Sender.Canvas.Brush.Color:=FAlterColor;
    if Text = '' then exit;
    case ADataCol of
      0:
        begin
          Indent:=integer(Sender.Items[idxAtLevel, ARow])*16;
          Options:=[coDrawCheckBox];
          i:=integer(Sender.Items[idxAtChecked, ARow]);
          if i = 0 then
            State:=cbUnchecked
          else
            if i = 2 then
              State:=cbGrayed
            else
              State:=cbChecked;
          if VarIsEmpty(Sender.Items[idxAtFileID, ARow]) then begin
            Include(Options, coDrawTreeButton);
            Expanded:=integer(Sender.Items[idxAtCollapsed, ARow]) = 0;
            ImageIndex:=22;
          end
          else
            if HasFolders then
              Inc(Indent, Sender.RowHeights[ARow + Sender.FixedRows]);
        end;
      1:
        Text:=GetHumanSize(double(Sender.Items[ADataCol, ARow]));
    end;
  end;
end;

procedure TAddTorrentForm.lvFilesCheckBoxClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
var
  i, st, lev: integer;
begin
  if integer(Sender.Items[idxAtChecked, ARow]) = 1 then
    st:=0
  else
    st:=1;
  Sender.Items[idxAtChecked, ARow]:=st;
  lev:=integer(Sender.Items[idxAtLevel, ARow]);

  if VarIsEmpty(Sender.Items[idxAtFileID, ARow]) then begin
    Sender.Items.BeginUpdate;
    for i:=ARow + 1 to Sender.Items.Count - 1 do
      if integer(Sender.Items[idxAtLevel, i]) <= lev then
        break
      else
        Sender.Items[idxAtChecked, i]:=st;
    Sender.Items.EndUpdate;
  end;

  if lev > 0 then begin
    i:=ARow + 1;
    while (i < Sender.Items.Count) and (integer(Sender.Items[idxAtLevel, i]) >= lev) do
      Inc(i);

    for i:=i - 1 downto 0 do begin
      if VarIsEmpty(Sender.Items[idxAtFileID, i]) and (integer(Sender.Items[idxAtLevel, i]) < lev) then begin
        Sender.Items[idxAtChecked, i]:=st;
        Dec(lev);
        if lev = 0 then
          break;
      end
      else
        if integer(Sender.Items[idxAtChecked, i]) <> st then
          st:=2;
    end;
  end;
  UpdateSize;
end;

procedure TAddTorrentForm.lvFilesTreeButtonClick(Sender: TVarGrid; ACol, ARow, ADataCol: integer);
begin
  if integer(Sender.Items[idxAtCollapsed, ARow]) = 0 then
    CollapseFolder(ARow)
  else
    ExpandFolder(ARow);
end;

procedure TAddTorrentForm.CollapseFolder(ARow: integer);
var
  i, lev: integer;
begin
  lvFiles.Items.BeginUpdate;
  lev:=lvFiles.Items[idxAtLevel, ARow];
  lvFiles.Items[idxAtCollapsed, ARow]:=1;
  for i:=ARow + 1 to lvFiles.Items.Count - 1 do
    if integer(lvFiles.Items[idxAtLevel, i]) > lev then
      lvFiles.RowHeights[i + lvFiles.FixedRows]:=0
    else
      break;
  TreeChanged;
  lvFiles.Items.EndUpdate;
end;

procedure TAddTorrentForm.ExpandFolder(ARow: integer);
var
  i, j, lev: integer;
begin
  lvFiles.Items.BeginUpdate;
  lev:=lvFiles.Items[idxAtLevel, ARow] + 1;
  lvFiles.Items[idxAtCollapsed, ARow]:=0;
  for i:=ARow + 1 to lvFiles.Items.Count - 1 do begin
    j:=integer(lvFiles.Items[idxAtLevel, i]);
    if j = lev then begin
      lvFiles.RowHeights[i + lvFiles.FixedRows]:=lvFiles.DefaultRowHeight;
      if VarIsEmpty(lvFiles.Items[idxAtFileID, i]) and (integer(lvFiles.Items[idxAtCollapsed, i]) = 0) then
        ExpandFolder(i);
    end
    else
      if j <= lev then
        break;
  end;
  TreeChanged;
  lvFiles.Items.EndUpdate;
end;

procedure TAddTorrentForm.TreeChanged;
var
  i, j: integer;
begin
  lvFiles.Items.BeginUpdate;
  j:=0;
  for i:=0 to lvFiles.Items.Count - 1 do begin
    if lvFiles.RowVisible[i] then begin
      lvFiles.Items[idxAtIndex, i]:=j;
      Inc(j);
    end;
  end;
  lvFiles.Items.EndUpdate;
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
    if not VarIsEmpty(lvFiles.Items[idxAtFileID, i]) then begin
      d:=double(lvFiles.Items[idxAtSize, i]);
      tsz:=tsz + d;
      if integer(lvFiles.Items[idxAtChecked, i]) = 1 then
        sz:=sz + d;
    end;

  s:=GetHumanSize(sz);
  if s <> GetHumanSize(tsz) then
    s:=s + ' / ' + GetHumanSize(tsz);
  txSize.Caption:=Format('%s: %s', [SSize, s]);
end;

procedure TAddTorrentForm.btSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  lvFiles.Items.BeginUpdate;
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[idxAtChecked, i]:=1;
  lvFiles.Items.EndUpdate;
  UpdateSize;
end;

procedure TAddTorrentForm.btSelectNoneClick(Sender: TObject);
var
  i: integer;
begin
  lvFiles.Items.BeginUpdate;
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[idxAtChecked, i]:=0;
  lvFiles.Items.EndUpdate;
  UpdateSize;
end;

procedure TAddTorrentForm.FormCreate(Sender: TObject);
begin
  Font.Size:=MainForm.Font.Size;
  lvFiles.Items.ExtraColumns:=5;
  lvFiles.OnCheckBoxClick:=@lvFilesCheckBoxClick;
  lvFiles.OnTreeButtonClick:=@lvFilesTreeButtonClick;
{$ifdef windows}
  gbSaveAs.Caption:='';
{$endif windows}
end;

initialization
  {$I addtorrent.lrs}

end.

