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

unit ConnOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ComCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbTrayIconAlways: TCheckBox;
    cbTrayMinimize: TCheckBox;
    cbTrayClose: TCheckBox;
    edHost: TEdit;
    edRefreshInterval: TSpinEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    gbTray: TGroupBox;
    Page: TPageControl;
    tabConnection: TTabSheet;
    tabInterface: TTabSheet;
    txSeconds: TLabel;
    txRefreshInterval: TLabel;
    txUserName: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txHost: TLabel;
    txPassword: TLabel;
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{ TOptionsForm }

procedure TOptionsForm.btOKClick(Sender: TObject);
begin
  edHost.Text:=Trim(edHost.Text);
  if edHost.Text = '' then begin
    Page.ActivePage:=tabConnection;
    edHost.SetFocus;
    MessageDlg('No host name specified.', mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Page.ActivePageIndex:=0;
  ActiveControl:=edHost;
end;

initialization
  {$I connoptions.lrs}

end.

