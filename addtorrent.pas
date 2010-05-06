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

unit AddTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Spin;

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
    lvFiles: TListView;
    txDestFolder: TLabel;
    procedure btSelectAllClick(Sender: TObject);
    procedure btSelectNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
  public
    { public declarations }
  end; 

implementation

uses lclintf, lcltype;

{ TAddTorrentForm }

procedure TAddTorrentForm.lvFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$ifdef windows}
var
  it: TListItem;
  R: TRect;
{$endif}
begin
{$ifdef windows}
  it:=TListView(Sender).GetItemAt(X, Y);
  if it <> nil then begin
    R:=it.DisplayRect(drBounds);
    R.Right:=R.Left + (R.Bottom - R.Top);
    if PtInRect(R, Point(X, Y)) then
      it.Checked:=not it.Checked;
  end;
{$endif}
end;

procedure TAddTorrentForm.FormShow(Sender: TObject);
begin
  btSelectAllClick(nil);
end;

procedure TAddTorrentForm.lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then begin
    Key:=0;
    if lvFiles.Selected = nil then
      exit;
    lvFiles.Selected.Checked:=not lvFiles.Selected.Checked;
  end;
end;

procedure TAddTorrentForm.btSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[i].Checked:=True;
end;

procedure TAddTorrentForm.btSelectNoneClick(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[i].Checked:=False;
end;

procedure TAddTorrentForm.FormCreate(Sender: TObject);
begin
{$ifdef windows}
  gbSaveAs.Caption:='';
{$endif windows}
end;

initialization
  {$I addtorrent.lrs}

end.

