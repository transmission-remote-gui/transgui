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

unit MoveTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

resourcestring
  SNoTorrentDir = 'No torrent location was specified.';

type

  { TMoveTorrentForm }

  TMoveTorrentForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbMoveData: TCheckBox;
    edTorrentDir: TComboBox;
    txTorrentDir: TLabel;
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

procedure TMoveTorrentForm.FormCreate(Sender: TObject);
begin
  Font.Size:=MainForm.Font.Size;
end;

initialization
  {$I movetorrent.lrs}

end.

