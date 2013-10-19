{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2013 by Yury Sidorov.

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

unit MoveTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, ExtCtrls, BaseForm;

resourcestring
  SNoTorrentDir = 'No torrent location was specified.';
  SSelectFolder = 'Select torrent location';

type

  { TMoveTorrentForm }

  TMoveTorrentForm = class(TBaseForm)
    btBrowse: TButton;
    Buttons: TButtonPanel;
    cbMoveData: TCheckBox;
    edTorrentDir: TComboBox;
    Panel1: TPanel;
    txTorrentDir: TLabel;
    procedure btBrowseClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses main;

{ TMoveTorrentForm }

procedure TMoveTorrentForm.btOKClick(Sender: TObject);
begin
  edTorrentDir.Text:=Trim(edTorrentDir.Text);
  if edTorrentDir.Text = '' then begin
    edTorrentDir.SetFocus;
    MessageDlg(SNoTorrentDir, mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

procedure TMoveTorrentForm.btBrowseClick(Sender: TObject);
var
  s: string;
begin
  s:=MainForm.SelectRemoteFolder(edTorrentDir.Text, SSelectFolder);
  if s <> '' then
    edTorrentDir.Text:=s;
end;

procedure TMoveTorrentForm.FormCreate(Sender: TObject);
begin
  Buttons.OKButton.ModalResult:=mrNone;
  Buttons.OKButton.OnClick:=@btOKClick;
end;

initialization
  {$I movetorrent.lrs}

end.

