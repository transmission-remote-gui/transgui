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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ComCtrls, Buttons;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    btDelHost: TBitBtn;
    btCancel: TButton;
    btOK: TButton;
    cbTrayIconAlways: TCheckBox;
    cbTrayMinimize: TCheckBox;
    cbTrayClose: TCheckBox;
    cbUseProxy: TCheckBox;
    cbShowAddTorrentWindow: TCheckBox;
    cbHost: TComboBox;
    edProxyPassword: TEdit;
    edProxyPort: TSpinEdit;
    edProxy: TEdit;
    edRefreshInterval: TSpinEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edProxyUserName: TEdit;
    gbTray: TGroupBox;
    edPaths: TMemo;
    txPaths: TLabel;
    tabPaths: TTabSheet;
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
    procedure btDelHostClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure cbHostSelect(Sender: TObject);
    procedure cbUseProxyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tabConnectionShow(Sender: TObject);
    procedure tabPathsShow(Sender: TObject);
  private
    FCurHost: string;
  public
    procedure LoadHostSettings(const HostName: string);
    procedure SaveHostSettings(const HostName: string);
    function IsHostSettingsChanged(const HostName: string): boolean;
  end;

implementation

uses Main, synacode;

{ TOptionsForm }

procedure TOptionsForm.btOKClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  cbHost.Text:=Trim(cbHost.Text);
  if Trim(cbHost.Text) = '' then begin
    Page.ActivePage:=tabConnection;
    cbHost.SetFocus;
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
  s:=cbHost.Text;
  i:=cbHost.Items.IndexOf(s);
  if i >= 0 then
    cbHost.Items.Move(i, 0);
  cbHost.Text:=s;
  ModalResult:=mrOk;
end;

procedure TOptionsForm.cbHostSelect(Sender: TObject);
var
  i: integer;
  s: string;
begin
  cbHost.Text:=Trim(cbHost.Text);
  i:=cbHost.ItemIndex;
  if i >= 0 then
    s:=cbHost.Items[i]
  else
    s:=cbHost.Text;

  if (FCurHost <> s) and (FCurHost <> '') then
    SaveHostSettings(FCurHost);
  LoadHostSettings(s);
  FCurHost:=s;
end;

procedure TOptionsForm.btDelHostClick(Sender: TObject);
var
  i: integer;
begin
  cbHost.Text:=Trim(cbHost.Text);
  if cbHost.Text = '' then
    exit;
  MainForm.Ini.EraseSection('Connection.' + cbHost.Text);
  MainForm.Ini.EraseSection('Connection');
  MainForm.Ini.EraseSection('AddTorrent.' + cbHost.Text);
  i:=cbHost.Items.IndexOf(cbHost.Text);
  if i >= 0 then begin
    cbHost.Items.Delete(i);
    if i >= cbHost.Items.Count then begin
      i:=cbHost.Items.Count - 1;
      if i < 0 then
        i:=0;
    end;
  end
  else
    i:=0;
  if i < cbHost.Items.Count then begin
    cbHost.ItemIndex:=i;
    cbHost.Text:=cbHost.Items[i];
  end
  else
    cbHost.Text:='';
  LoadHostSettings(cbHost.Text);
end;

procedure TOptionsForm.cbUseProxyClick(Sender: TObject);
begin
  edProxy.Enabled:=cbUseProxy.Checked;
  edProxyPort.Enabled:=cbUseProxy.Checked;
  edProxyUserName.Enabled:=cbUseProxy.Checked;
  edProxyPassword.Enabled:=cbUseProxy.Checked;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
  tabConnectionShow(nil);
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Page.ActivePageIndex:=0;
  ActiveControl:=cbHost;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  cbUseProxyClick(nil);
  FCurHost:=cbHost.Text;
end;

procedure TOptionsForm.tabConnectionShow(Sender: TObject);
begin
  btDelHost.Height:=cbHost.Height + 2;
end;

procedure TOptionsForm.tabPathsShow(Sender: TObject);
var
  R: TRect;
begin
  R:=edPaths.BoundsRect;
  R.Top:=txPaths.BoundsRect.Bottom + 8;
  edPaths.BoundsRect:=R;
end;

procedure TOptionsForm.LoadHostSettings(const HostName: string);
var
  Sec: string;
begin
  with MainForm.Ini do begin
    Sec:='Connection.' + HostName;
    if not SectionExists(Sec) then
      Sec:='Connection';
    edPort.Value:=ReadInteger(Sec, 'Port', 9091);
    edUserName.Text:=ReadString(Sec, 'UserName', '');
    if ReadString(Sec, 'Password', '') <> '' then
      edPassword.Text:='******';
    cbUseProxy.Checked:=ReadBool(Sec, 'UseProxy', False);
    edProxy.Text:=ReadString(Sec, 'ProxyHost', '');
    edProxyPort.Value:=ReadInteger(Sec, 'ProxyPort', 8080);
    edProxyUserName.Text:=ReadString(Sec, 'ProxyUser', '');
    if ReadString(Sec, 'ProxyPass', '') <> '' then
      edProxyPassword.Text:='******';
    edPaths.Text:=StringReplace(ReadString(Sec, 'PathMap', ''), '|', LineEnding, [rfReplaceAll]);
  end;
end;

procedure TOptionsForm.SaveHostSettings(const HostName: string);
var
  Sec: string;
  i: integer;
  s: string;
begin
  with MainForm.Ini do begin
    Sec:='Connection.' + HostName;
    WriteString(Sec, 'Host', HostName);
    WriteInteger(Sec, 'Port', edPort.Value);
    WriteString(Sec, 'UserName', edUserName.Text);
    if edPassword.Text <> '******' then begin
      if edPassword.Text = '' then
        s:=''
      else
        s:=EncodeBase64(edPassword.Text);
      WriteString(Sec, 'Password', s);
    end;
    WriteBool(Sec, 'UseProxy', cbUseProxy.Checked);
    WriteString(Sec, 'ProxyHost', edProxy.Text);
    WriteInteger(Sec, 'ProxyPort', edProxyPort.Value);
    WriteString(Sec, 'ProxyUser', edProxyUserName.Text);
    if edProxyPassword.Text <> '******' then begin
      if edProxyPassword.Text = '' then
        s:=''
      else
        s:=EncodeBase64(edProxyPassword.Text);
      WriteString(Sec, 'ProxyPass', s);
    end;
    WriteString(Sec, 'PathMap', StringReplace(edPaths.Text, LineEnding, '|', [rfReplaceAll]));
    i:=cbHost.Items.IndexOf(HostName);
    if i < 0 then
      cbHost.Items.Insert(0, HostName);
  end;
end;

function TOptionsForm.IsHostSettingsChanged(const HostName: string): boolean;
var
  Sec: string;
begin
  with MainForm.Ini do begin
    Sec:='Connection.' + HostName;
    if not SectionExists(Sec) then
      Sec:='Connection';
    Result:=(edPort.Value <> ReadInteger(Sec, 'Port', 9091)) or
            (edUserName.Text <> ReadString(Sec, 'UserName', '')) or
            ((ReadString(Sec, 'Password', '') = '') and (edPassword.Text <> '')) or
            ((ReadString(Sec, 'Password', '') <> '') and (edPassword.Text <> '******')) or
            (cbUseProxy.Checked <> ReadBool(Sec, 'UseProxy', False)) or
            (edProxy.Text <> ReadString(Sec, 'ProxyHost', '')) or
            (edProxyPort.Value <> ReadInteger(Sec, 'ProxyPort', 8080)) or
            (edProxyUserName.Text <> ReadString(Sec, 'ProxyUser', '')) or
            ((ReadString(Sec, 'ProxyPass', '') = '') and (edProxyPassword.Text <> '')) or
            ((ReadString(Sec, 'ProxyPass', '') <> '') and (edProxyPassword.Text <> '******'));
  end;
end;

initialization
  {$I connoptions.lrs}

end.

