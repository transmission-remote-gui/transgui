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

unit ConnOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TConnOptionsForm }

  TConnOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    edHost: TEdit;
    edRefreshInterval: TSpinEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    GroupBox1: TGroupBox;
    txSeconds: TLabel;
    txRefreshInterval: TLabel;
    txUserName: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txHost: TLabel;
    txPassword: TLabel;
    procedure btOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{ TConnOptionsForm }

procedure TConnOptionsForm.btOKClick(Sender: TObject);
begin
  edHost.Text:=Trim(edHost.Text);
  if edHost.Text = '' then begin
    edHost.SetFocus;
    MessageDlg('No host name specified.', mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOk;
end;

initialization
  {$I connoptions.lrs}

end.

