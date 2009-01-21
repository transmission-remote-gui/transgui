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
    cbUseProxy: TCheckBox;
    edHost: TEdit;
    edProxyPassword: TEdit;
    edProxyPort: TSpinEdit;
    edProxy: TEdit;
    edRefreshInterval: TSpinEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edProxyUserName: TEdit;
    gbTray: TGroupBox;
    txProxyPassword: TLabel;
    txProxyPort: TLabel;
    txProxy: TLabel;
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
    txProxyUserName: TLabel;
    procedure btOKClick(Sender: TObject);
    procedure cbUseProxyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  edProxy.Text:=Trim(edProxy.Text);
  if cbUseProxy.Checked and (edProxy.Text = '') then begin
    Page.ActivePage:=tabConnection;
    edProxy.SetFocus;
    MessageDlg('No proxy server specified.', mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TOptionsForm.cbUseProxyClick(Sender: TObject);
begin
  edProxy.Enabled:=cbUseProxy.Checked;
  edProxyPort.Enabled:=cbUseProxy.Checked;
  edProxyUserName.Enabled:=cbUseProxy.Checked;
  edProxyPassword.Enabled:=cbUseProxy.Checked;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Page.ActivePageIndex:=0;
  ActiveControl:=edHost;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  cbUseProxyClick(nil);
end;

initialization
  {$I connoptions.lrs}

end.

