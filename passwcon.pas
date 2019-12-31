{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

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

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}
unit passwcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TPasswordConnect }

  TPasswordConnect = class(TForm)
    Buttons: TButtonPanel;
    passw: TEdit;
    lMsg: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SetText(form : string; msg : string);

  private
    { private declarations }
  public
    { public declarations }
    gPassw : string;
  end;

var
  PasswordConnect: TPasswordConnect;

implementation

uses main;

{ TPasswordConnect }

procedure TPasswordConnect.CancelButtonClick(Sender: TObject);
begin

end;

procedure TPasswordConnect.FormCreate(Sender: TObject);
begin

  Buttons.OKButton.ModalResult:= mrNone;
  bidiMode := GetBiDi;
  gPassw := '';
  passw.Text := '';
end;

procedure TPasswordConnect.OKButtonClick(Sender: TObject);
begin
  gPassw:= passw.Text;
  ModalResult:=mrOk;
end;


procedure TPasswordConnect.SetText(form : string; msg : string);
begin
  lMsg.Caption := msg;
  Caption := form;
end;


initialization
  {$I passwcon.lrs}

end.

