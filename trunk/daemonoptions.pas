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

unit DaemonOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls;

resourcestring
 sPortTestSuccess = 'Incoming port tested successfully.';
 sPortTestFailed = 'Incoming port is closed. Check your firewall settings.';
 sEncryptionDisabled = 'Encryption disabled';
 sEncryptionEnabled = 'Encryption enabled';
 sEncryptionRequired = 'Encryption required';
 SNoDownloadDir = 'The downloads directory was not specified.';
 SNoIncompleteDir = 'The directory for incomplete files was not specified.';

type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    btTestPort: TButton;
    cbBlocklist: TCheckBox;
    cbDHT: TCheckBox;
    cbEncryption: TComboBox;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbPEX: TCheckBox;
    cbPortForwarding: TCheckBox;
    cbRandomPort: TCheckBox;
    cbIncompleteDir: TCheckBox;
    cbPartExt: TCheckBox;
    cbSeedRatio: TCheckBox;
    edDownloadDir: TEdit;
    edIncompleteDir: TEdit;
    edMaxDown: TSpinEdit;
    edMaxPeers: TSpinEdit;
    edMaxUp: TSpinEdit;
    edPort: TSpinEdit;
    edSeedRatio: TFloatSpinEdit;
    gbBandwidth: TGroupBox;
    Page: TPageControl;
    tabNetwork: TTabSheet;
    tabBandwidth: TTabSheet;
    tabDownload: TTabSheet;
    txDownloadDir: TLabel;
    txEncryption: TLabel;
    txKbs1: TLabel;
    txKbs2: TLabel;
    txPeerLimit: TLabel;
    txPort: TLabel;
    procedure btOKClick(Sender: TObject);
    procedure btTestPortClick(Sender: TObject);
    procedure cbIncompleteDirClick(Sender: TObject);
    procedure cbMaxDownClick(Sender: TObject);
    procedure cbMaxUpClick(Sender: TObject);
    procedure cbRandomPortChange(Sender: TObject);
    procedure cbRandomPortClick(Sender: TObject);
    procedure cbSeedRatioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses main, utils, fpjson;

{ TDaemonOptionsForm }

procedure TDaemonOptionsForm.cbMaxDownClick(Sender: TObject);
begin
  edMaxDown.Enabled:=cbMaxDown.Checked;
end;

procedure TDaemonOptionsForm.btTestPortClick(Sender: TObject);
var
  req, res: TJSONObject;
begin
  AppBusy;
  req:=TJSONObject.Create;
  try
    req.Add('method', 'port-test');
    res:=RpcObj.SendRequest(req, False);
    AppNormal;
    if res = nil then
      MainForm.CheckStatus(False)
    else
      if res.Objects['arguments'].Integers['port-is-open'] <> 0 then
        MessageDlg(sPortTestSuccess, mtInformation, [mbOk], 0)
      else
        MessageDlg(sPortTestFailed, mtError, [mbOK], 0);
    res.Free;
  finally
    req.Free;
  end;
end;

procedure TDaemonOptionsForm.btOKClick(Sender: TObject);
begin
  edDownloadDir.Text:=Trim(edDownloadDir.Text);
  if edDownloadDir.Text = '' then begin
    Page.ActivePage:=tabDownload;
    edDownloadDir.SetFocus;
    MessageDlg(SNoDownloadDir, mtError, [mbOK], 0);
    exit;
  end;
  edIncompleteDir.Text:=Trim(edIncompleteDir.Text);
  if cbIncompleteDir.Checked and (edIncompleteDir.Text = '') then begin
    Page.ActivePage:=tabDownload;
    edIncompleteDir.SetFocus;
    MessageDlg(SNoIncompleteDir, mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

procedure TDaemonOptionsForm.cbIncompleteDirClick(Sender: TObject);
begin
  edIncompleteDir.Enabled:=cbIncompleteDir.Checked;
  if edIncompleteDir.Enabled then
    edIncompleteDir.Color:=clWindow
  else
    edIncompleteDir.ParentColor:=True;
end;

procedure TDaemonOptionsForm.cbMaxUpClick(Sender: TObject);
begin
  edMaxUp.Enabled:=cbMaxUp.Checked;
end;

procedure TDaemonOptionsForm.cbRandomPortChange(Sender: TObject);
begin
end;

procedure TDaemonOptionsForm.cbRandomPortClick(Sender: TObject);
begin
  edPort.Enabled:=not cbRandomPort.Checked;
end;

procedure TDaemonOptionsForm.cbSeedRatioClick(Sender: TObject);
begin
  edSeedRatio.Enabled:=cbSeedRatio.Checked;
end;

procedure TDaemonOptionsForm.FormCreate(Sender: TObject);
begin
  Font.Size:=MainForm.Font.Size;
  Page.ActivePageIndex:=0;
  cbEncryption.Items.Add(sEncryptionDisabled);
  cbEncryption.Items.Add(sEncryptionEnabled);
  cbEncryption.Items.Add(sEncryptionRequired);
end;

initialization
  {$I daemonoptions.lrs}

end.

