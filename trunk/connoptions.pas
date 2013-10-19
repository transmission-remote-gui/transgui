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

unit ConnOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ComCtrls, Buttons, ButtonPanel, ExtCtrls, BaseForm;

const
  DefSpeeds = '0,10,25,50,100,250,500,750,1000,2500,5000,7000';

resourcestring
  sNoHost = 'No host name specified.';
  sNoProxy = 'No proxy server specified.';
  SDelConnection = 'Are you sure to delete connection ''%s''?';
  SNewConnection = 'New connection to Transmission';

type

  { TConnOptionsForm }

  TConnOptionsForm = class(TBaseForm)
    btNew: TButton;
    btDel: TButton;
    btRename: TButton;
    Buttons: TButtonPanel;
    cbProxyAuth: TCheckBox;
    cbUseProxy: TCheckBox;
    cbUseSocks5: TCheckBox;
    cbAuth: TCheckBox;
    cbShowAdvanced: TCheckBox;
    cbAskPassword: TCheckBox;
    edRpcPath: TEdit;
    edUpSpeeds: TEdit;
    edHost: TEdit;
    cbSSL: TCheckBox;
    cbConnection: TComboBox;
    edDownSpeeds: TEdit;
    edProxy: TEdit;
    edProxyPassword: TEdit;
    edProxyPort: TSpinEdit;
    edProxyUserName: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edPaths: TMemo;
    gbSpeed: TGroupBox;
    txRpcPath: TLabel;
    txConName: TLabel;
    txConnHelp: TLabel;
    txDownSpeeds: TLabel;
    panTop: TPanel;
    tabProxy: TTabSheet;
    tabMisc: TTabSheet;
    txUpSpeeds: TLabel;
    txPaths: TLabel;
    tabPaths: TTabSheet;
    Page: TPageControl;
    tabConnection: TTabSheet;
    txProxy: TLabel;
    txProxyPassword: TLabel;
    txProxyPort: TLabel;
    txProxyUserName: TLabel;
    txUserName: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txHost: TLabel;
    txPassword: TLabel;
    procedure btDelClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btRenameClick(Sender: TObject);
    procedure cbAskPasswordClick(Sender: TObject);
    procedure cbAuthClick(Sender: TObject);
    procedure cbConnectionSelect(Sender: TObject);
    procedure cbProxyAuthClick(Sender: TObject);
    procedure cbShowAdvancedClick(Sender: TObject);
    procedure cbUseProxyClick(Sender: TObject);
    procedure edHostChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tabPathsShow(Sender: TObject);
  private
    FCurConn: string;
    FCurHost: string;
    edConnection: TEdit;

    function Validate: boolean;
    procedure BeginEdit;
    procedure EndEdit;
    procedure SaveConnectionsList;
  public
    ActiveConnection: string;
    ActiveSettingChanged: boolean;

    procedure LoadConnSettings(const ConnName: string);
    procedure SaveConnSettings(const ConnName: string);
    function IsConnSettingsChanged(const ConnName: string): boolean;
  end;

implementation

uses Main, synacode, utils, rpc;

{ TConnOptionsForm }

procedure TConnOptionsForm.btOKClick(Sender: TObject);
begin
  if not Validate then
    exit;
  EndEdit;
  SaveConnSettings(FCurConn);
  SaveConnectionsList;
  ModalResult:=mrOk;
end;

procedure TConnOptionsForm.btRenameClick(Sender: TObject);
begin
  if edConnection.Visible then begin
    if Trim(edConnection.Text) = '' then exit;
    EndEdit;
    exit;
  end;
  if cbConnection.Text = '' then exit;
  BeginEdit;
  ActiveControl:=edConnection;
  edConnection.SelectAll;
end;

procedure TConnOptionsForm.cbAskPasswordClick(Sender: TObject);
begin
  EnableControls(not cbAskPassword.Checked and cbAskPassword.Enabled, [txPassword, edPassword]);
end;

procedure TConnOptionsForm.cbAuthClick(Sender: TObject);
begin
  EnableControls(cbAuth.Checked, [txUserName, edUserName, txPassword, cbAskPassword]);
  cbAskPasswordClick(nil);
end;

procedure TConnOptionsForm.cbConnectionSelect(Sender: TObject);
var
  i: integer;
  s: string;
begin
  if edConnection.Visible then
    exit;
  i:=cbConnection.ItemIndex;
  if i >= 0 then
    s:=cbConnection.Items[i]
  else
    s:='';

  if (FCurConn <> s) and (FCurConn <> '') then begin
    if not Validate then begin
      cbConnection.ItemIndex:=cbConnection.Items.IndexOf(FCurConn);
      exit;
    end;
    SaveConnSettings(FCurConn);
  end;
  if s <> '' then
    LoadConnSettings(s);
end;

procedure TConnOptionsForm.cbProxyAuthClick(Sender: TObject);
begin
  EnableControls(cbProxyAuth.Checked and cbProxyAuth.Enabled, [txProxyUserName, edProxyUserName, txProxyPassword, edProxyPassword]);
end;

procedure TConnOptionsForm.cbShowAdvancedClick(Sender: TObject);
begin
  txRpcPath.Visible:=cbShowAdvanced.Checked;
  edRpcPath.Visible:=cbShowAdvanced.Checked;
  tabConnection.TabVisible:=cbShowAdvanced.Checked;
  tabProxy.TabVisible:=cbShowAdvanced.Checked;
  tabPaths.TabVisible:=cbShowAdvanced.Checked;
  tabMisc.TabVisible:=cbShowAdvanced.Checked;
  cbShowAdvanced.Visible:=not cbShowAdvanced.Checked;
  Page.ActivePage:=tabConnection;
end;

procedure TConnOptionsForm.btNewClick(Sender: TObject);
begin
  EndEdit;
  if (FCurConn <> '') and not Validate then
    exit;
  SaveConnSettings(FCurConn);
  LoadConnSettings('');
  BeginEdit;
  edConnection.Text:='';
  Page.ActivePage:=tabConnection;
  ActiveControl:=edHost;
end;

procedure TConnOptionsForm.btDelClick(Sender: TObject);
var
  i: integer;
begin
  if edConnection.Visible or (cbConnection.Text = '') then
    exit;
  if MessageDlg('', Format(SDelConnection, [cbConnection.Text]), mtConfirmation, mbYesNo, 0, mbNo) <> mrYes then exit;
  if FCurConn <> '' then begin
    Ini.EraseSection('Connection.' + FCurConn);
    Ini.EraseSection('Connection');
    Ini.EraseSection('AddTorrent.' + FCurConn);

    i:=cbConnection.ItemIndex;
    if i >= 0 then begin
      cbConnection.Items.Delete(i);
      if i >= cbConnection.Items.Count then begin
        i:=cbConnection.Items.Count - 1;
        if i < 0 then
          i:=0;
      end;
    end
    else
      i:=0;
    if i < cbConnection.Items.Count then
      cbConnection.ItemIndex:=i
    else
      cbConnection.ItemIndex:=-1;
  end
  else
    cbConnection.ItemIndex:=-1;
  if cbConnection.ItemIndex >= 0 then begin
    if FCurConn = ActiveConnection then
      ActiveConnection:='';
    LoadConnSettings(cbConnection.Items[cbConnection.ItemIndex]);
    if ActiveConnection = '' then
      ActiveConnection:=FCurConn;
  end
  else begin
    FCurConn:='';
    btNewClick(nil);
  end;
  SaveConnectionsList;
end;

procedure TConnOptionsForm.cbUseProxyClick(Sender: TObject);
begin
  EnableControls(cbUseProxy.Checked, [txProxy, edProxy, txProxyPort, edProxyPort, cbUseSocks5, cbProxyAuth]);
  cbProxyAuthClick(nil);
end;

procedure TConnOptionsForm.edHostChange(Sender: TObject);
begin
  if edConnection.Visible and (edConnection.Text = FCurHost) then
    edConnection.Text:=edHost.Text;
  FCurHost:=edHost.Text;
end;

procedure TConnOptionsForm.FormCreate(Sender: TObject);
var
  i, cnt: integer;
  s: string;
begin
  Page.ActivePageIndex:=0;
  txConnHelp.Caption:=Format(txConnHelp.Caption, [AppName]);
  ActiveControl:=edHost;
  Buttons.OKButton.ModalResult:=mrNone;
  Buttons.OKButton.OnClick:=@btOKClick;

  edConnection:=TEdit.Create(cbConnection.Parent);
  edConnection.Visible:=False;
  edConnection.BoundsRect:=cbConnection.BoundsRect;
  edConnection.Parent:=cbConnection.Parent;

  cnt:=Ini.ReadInteger('Hosts', 'Count', 0);
  for i:=1 to cnt do begin
    s:=Ini.ReadString('Hosts', Format('Host%d', [i]), '');
    if s <> '' then
      cbConnection.Items.Add(s);
  end;

  cbShowAdvanced.Top:=edRpcPath.Top;
end;

procedure TConnOptionsForm.FormShow(Sender: TObject);
begin
  if edConnection.Visible then
    exit;
  if cbConnection.Items.Count = 0 then begin
    btNewClick(nil);
    exit;
  end;
  cbConnection.ItemIndex:=cbConnection.Items.IndexOf(ActiveConnection);
  if cbConnection.ItemIndex < 0 then
    cbConnection.ItemIndex:=0;
  LoadConnSettings(cbConnection.Text);
end;

procedure TConnOptionsForm.tabPathsShow(Sender: TObject);
var
  R: TRect;
begin
  R:=edPaths.BoundsRect;
  R.Top:=txPaths.BoundsRect.Bottom + 8;
  edPaths.BoundsRect:=R;
end;

function TConnOptionsForm.Validate: boolean;
begin
  Result:=False;
  edHost.Text:=Trim(edHost.Text);
  if Trim(edHost.Text) = '' then begin
    Page.ActivePage:=tabConnection;
    edHost.SetFocus;
    MessageDlg(sNoHost, mtError, [mbOK], 0);
    exit;
  end;
  edProxy.Text:=Trim(edProxy.Text);
  if tabProxy.TabVisible and cbUseProxy.Checked and (edProxy.Text = '') then begin
    Page.ActivePage:=tabProxy;
    edProxy.SetFocus;
    MessageDlg(sNoProxy, mtError, [mbOK], 0);
    exit;
  end;
  Result:=True;
end;

procedure TConnOptionsForm.EndEdit;

  procedure RenameSection(const OldName, NewName: string);
  var
    i: integer;
    sl: TStringList;
  begin
    sl:=TStringList.Create;
    with Ini do
    try
      ReadSectionValues(OldName, sl);
      for i:=0 to sl.Count - 1 do
        WriteString(NewName, sl.Names[i], sl.ValueFromIndex[i]);
      EraseSection(OldName);
    finally
      sl.Free;
    end;
  end;

var
  NewName, s: string;
  i, p: integer;
begin
  if not edConnection.Visible then exit;
  NewName:=Trim(edConnection.Text);
  if NewName = '' then
    NewName:=Trim(edHost.Text);
  if NewName <> FCurConn then begin
    if FCurConn <> '' then begin
      p:=cbConnection.Items.IndexOf(FCurConn);
      if p >= 0 then
        cbConnection.Items.Delete(p);
    end
    else
      p:=-1;

    i:=1;
    s:=NewName;
    while cbConnection.Items.IndexOf(NewName) >= 0 do begin
      Inc(i);
      NewName:=Format('%s (%d)', [s, i]);
    end;

    if FCurConn <> '' then begin
      RenameSection('Connection.' + FCurConn, 'Connection.' + NewName);
      RenameSection('AddTorrent.' + FCurConn, 'AddTorrent.' + NewName);
    end;

    if p >= 0 then
      cbConnection.Items.Insert(p, NewName)
    else
      cbConnection.Items.Add(NewName);
    if (FCurConn = ActiveConnection) or (FCurConn = '') then
      ActiveConnection:=NewName;
    FCurConn:=NewName;
    SaveConnectionsList;
  end;
  cbConnection.ItemIndex:=cbConnection.Items.IndexOf(NewName);
  cbConnection.Visible:=True;
  edConnection.Visible:=False;
end;

procedure TConnOptionsForm.SaveConnectionsList;
var
  i: integer;
begin
  with Ini do begin
    WriteString('Hosts', 'CurHost', ActiveConnection);
    WriteInteger('Hosts', 'Count', cbConnection.Items.Count);
    for i:=0 to cbConnection.Items.Count - 1 do
      WriteString('Hosts', Format('Host%d', [i + 1]), cbConnection.Items[i]);
    UpdateFile;
  end;
end;

procedure TConnOptionsForm.BeginEdit;
var
  i: integer;
begin
  i:=cbConnection.ItemIndex;
  if i >= 0 then
    edConnection.Text:=cbConnection.Items[i]
  else
    edConnection.Text:='';
  edConnection.Visible:=True;
  cbConnection.Visible:=False;
end;

procedure TConnOptionsForm.LoadConnSettings(const ConnName: string);
var
  Sec, s: string;
begin
  with Ini do begin
    Sec:='Connection.' + ConnName;
    if (ConnName <> '') and not SectionExists(Sec) then
      Sec:='Connection';
    edHost.Text:=ReadString(Sec, 'Host', '');
    FCurHost:=edHost.Text;
    edPort.Value:=ReadInteger(Sec, 'Port', 9091);
    cbSSL.Checked:=ReadBool(Sec, 'UseSSL', False);
    edUserName.Text:=ReadString(Sec, 'UserName', '');
    cbAuth.Checked:=edUserName.Text <> '';
    if cbAuth.Checked then begin
      s:=ReadString(Sec, 'Password', '');
      cbAskPassword.Checked:=s = '-';
      if not cbAskPassword.Checked then
        if s <> '' then
          edPassword.Text:='******'
        else
          edPassword.Text:='';
    end;
    cbAuthClick(nil);
    edRpcPath.Text:=ReadString(Sec, 'RpcPath', DefaultRpcPath);
    cbUseProxy.Checked:=ReadBool(Sec, 'UseProxy', False);
    cbUseSocks5.Checked:=ReadBool(Sec, 'UseSockProxy', False);
    edProxy.Text:=ReadString(Sec, 'ProxyHost', '');
    edProxyPort.Value:=ReadInteger(Sec, 'ProxyPort', 8080);
    edProxyUserName.Text:=ReadString(Sec, 'ProxyUser', '');
    cbProxyAuth.Checked:=edProxyUserName.Text <> '';
    if cbProxyAuth.Checked then
      if ReadString(Sec, 'ProxyPass', '') <> '' then
        edProxyPassword.Text:='******'
      else
        edProxyPassword.Text:='';
    edPaths.Text:=StringReplace(ReadString(Sec, 'PathMap', ''), '|', LineEnding, [rfReplaceAll]);
    edDownSpeeds.Text:=ReadString(Sec, 'DownSpeeds', DefSpeeds);
    edUpSpeeds.Text:=ReadString(Sec, 'UpSpeeds', DefSpeeds);
    cbUseProxyClick(nil);
  end;
  FCurConn:=ConnName;
  FCurHost:=edHost.Text;
end;

procedure TConnOptionsForm.SaveConnSettings(const ConnName: string);
var
  Sec: string;
  i: integer;
  s: string;
begin
  if ConnName = '' then
    exit;
  if ConnName = ActiveConnection then
    if IsConnSettingsChanged(ConnName) then
      ActiveSettingChanged:=True;

  with Ini do begin
    Sec:='Connection.' + ConnName;
    WriteString(Sec, 'Host', Trim(edHost.Text));
    WriteBool(Sec, 'UseSSL', cbSSL.Checked);
    WriteInteger(Sec, 'Port', edPort.Value);
    if not cbAuth.Checked then begin
      edUserName.Text:='';
      edPassword.Text:='';
      cbAskPassword.Checked:=False;
    end;
    WriteString(Sec, 'UserName', edUserName.Text);
    if cbAskPassword.Checked then
      WriteString(Sec, 'Password', '-')
    else
      if edPassword.Text <> '******' then begin
        if edPassword.Text = '' then
          s:=''
        else
          s:=EncodeBase64(edPassword.Text);
        WriteString(Sec, 'Password', s);
      end;

    if (edRpcPath.Text = DefaultRpcPath) or (edRpcPath.Text = '') then
      DeleteKey(Sec, 'RpcPath')
    else
      WriteString(Sec, 'RpcPath', edRpcPath.Text);

    WriteBool(Sec, 'UseProxy', cbUseProxy.Checked);
    WriteBool(Sec, 'UseSockProxy', cbUseSocks5.Checked);
    WriteString(Sec, 'ProxyHost', Trim(edProxy.Text));
    WriteInteger(Sec, 'ProxyPort', edProxyPort.Value);
    if cbProxyAuth.Checked then begin
      edProxyUserName.Text:='';
      edProxyPassword.Text:='';
    end;
    WriteString(Sec, 'ProxyUser', edProxyUserName.Text);
    if edProxyPassword.Text <> '******' then begin
      if edProxyPassword.Text = '' then
        s:=''
      else
        s:=EncodeBase64(edProxyPassword.Text);
      WriteString(Sec, 'ProxyPass', s);
    end;
    WriteString(Sec, 'PathMap', StringReplace(edPaths.Text, LineEnding, '|', [rfReplaceAll]));
    WriteString(Sec, 'DownSpeeds', Trim(edDownSpeeds.Text));
    WriteString(Sec, 'UpSpeeds', Trim(edUpSpeeds.Text));

    i:=cbConnection.Items.IndexOf(ConnName);
    if i < 0 then
      cbConnection.Items.Insert(0, ConnName);
    UpdateFile;
  end;
end;

function TConnOptionsForm.IsConnSettingsChanged(const ConnName: string): boolean;
var
  Sec: string;
begin
  with Ini do begin
    Sec:='Connection.' + ConnName;
    if not SectionExists(Sec) then
      Sec:='Connection';
    Result:=(edPort.Value <> ReadInteger(Sec, 'Port', 9091)) or
            (edHost.Text <> ReadString(Sec, 'Host', '')) or
            (cbSSL.Checked <> ReadBool(Sec, 'UseSSL', False)) or
            (edUserName.Text <> ReadString(Sec, 'UserName', '')) or
            ((ReadString(Sec, 'Password', '') = '') and (edPassword.Text <> '')) or
            ((ReadString(Sec, 'Password', '') <> '') and (edPassword.Text <> '******')) or
            (edRpcPath.Text <> ReadString(Sec, 'RpcPath', DefaultRpcPath)) or
            (cbUseProxy.Checked <> ReadBool(Sec, 'UseProxy', False)) or
            (edProxy.Text <> ReadString(Sec, 'ProxyHost', '')) or
            (edProxyPort.Value <> ReadInteger(Sec, 'ProxyPort', 8080)) or
            (edProxyUserName.Text <> ReadString(Sec, 'ProxyUser', '')) or
            ((ReadString(Sec, 'ProxyPass', '') = '') and (edProxyPassword.Text <> '')) or
            ((ReadString(Sec, 'ProxyPass', '') <> '') and (edProxyPassword.Text <> '******')) or
            (edPaths.Text <> StringReplace(ReadString(Sec, 'PathMap', ''), '|', LineEnding, [rfReplaceAll])) or
            (edDownSpeeds.Text <> ReadString(Sec, 'DownSpeeds', '')) or
            (edUpSpeeds.Text <> ReadString(Sec, 'UpSpeeds', ''))
            ;
  end;
end;

initialization
  {$I connoptions.lrs}

end.

