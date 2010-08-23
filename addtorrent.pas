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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, VarGrid, Grids;

type

  { TAddTorrentForm }

  TAddTorrentForm = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    btSelectAll: TButton;
    btSelectNone: TButton;
    cbStartTorrent: TCheckBox;
    cbDestFolder: TComboBox;
    gbSaveAs: TGroupBox;
    gbContents: TGroupBox;
    edPeerLimit: TSpinEdit;
    txPeerLimit: TLabel;
    lvFiles: TVarGrid;
    txDestFolder: TLabel;
    procedure btSelectAllClick(Sender: TObject);
    procedure btSelectNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
  private
  public
    { public declarations }
  end;

implementation

uses lclintf, lcltype, main;

{ TAddTorrentForm }

procedure TAddTorrentForm.FormShow(Sender: TObject);
begin
  btSelectAllClick(nil);
  lvFiles.Sort;
  if lvFiles.Items.Count > 0 then
    lvFiles.Row:=0;
end;

procedure TAddTorrentForm.lvFilesCellAttributes(Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState;
  var CellAttribs: TCellAttributes);
begin
  if ARow < 0 then exit;
  with CellAttribs do begin
    if Text = '' then exit;
    case ADataCol of
      2:
        Text:=GetHumanSize(double(Sender.Items[ADataCol, ARow]));
    end;
  end;
end;

procedure TAddTorrentForm.btSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  lvFiles.Items.BeginUpdate;
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[0, i]:=1;
  lvFiles.Items.EndUpdate;
end;

procedure TAddTorrentForm.btSelectNoneClick(Sender: TObject);
var
  i: integer;
begin
  lvFiles.Items.BeginUpdate;
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[0, i]:=0;
  lvFiles.Items.EndUpdate;
end;

procedure TAddTorrentForm.FormCreate(Sender: TObject);
begin
  Font.Size:=MainForm.Font.Size;
  lvFiles.Items.ExtraColumns:=1;
  lvFiles.AlternateColor:=FAlterColor;
{$ifdef windows}
  gbSaveAs.Caption:='';
{$endif windows}
end;

initialization
  {$I addtorrent.lrs}

end.

