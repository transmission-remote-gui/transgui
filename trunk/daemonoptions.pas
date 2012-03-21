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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, CheckLst, EditBtn, MaskEdit,
  ButtonPanel, BaseForm;

resourcestring
 sPortTestSuccess = 'Incoming port tested successfully.';
 sPortTestFailed = 'Incoming port is closed. Check your firewall settings.';
 sEncryptionDisabled = 'Encryption disabled';
 sEncryptionEnabled = 'Encryption enabled';
 sEncryptionRequired = 'Encryption required';
 SNoDownloadDir = 'The downloads directory was not specified.';
 SNoIncompleteDir = 'The directory for incomplete files was not specified.';
 SNoBlocklistURL = 'The blocklist URL was not specified.';
 SInvalidTime = 'The invalid time value was entered.';

type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = class(TBaseForm)
    btTestPort: TButton;
    Buttons: TButtonPanel;
    cbBlocklist: TCheckBox;
    cbDHT: TCheckBox;
    cbUpQueue: TCheckBox;
    cbEncryption: TComboBox;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbPEX: TCheckBox;
    cbPortForwarding: TCheckBox;
    cbRandomPort: TCheckBox;
    cbIncompleteDir: TCheckBox;
    cbPartExt: TCheckBox;
    cbSeedRatio: TCheckBox;
    cbLPD: TCheckBox;
    cbIdleSeedLimit: TCheckBox;
    cbAltEnabled: TCheckBox;
    cbAutoAlt: TCheckBox;
    cbStalled: TCheckBox;
    cbUTP: TCheckBox;
    cbDownQueue: TCheckBox;
    edAltTimeEnd: TMaskEdit;
    edDownQueue: TSpinEdit;
    edUpQueue: TSpinEdit;
    edStalledTime: TSpinEdit;
    tabQueue: TTabSheet;
    txDays: TLabel;
    txFrom: TLabel;
    edDownloadDir: TEdit;
    edIncompleteDir: TEdit;
    edBlocklistURL: TEdit;
    edMaxDown: TSpinEdit;
    edAltDown: TSpinEdit;
    edMaxPeers: TSpinEdit;
    edMaxUp: TSpinEdit;
    edAltUp: TSpinEdit;
    edPort: TSpinEdit;
    edSeedRatio: TFloatSpinEdit;
    gbBandwidth: TGroupBox;
    edIdleSeedLimit: TSpinEdit;
    gbAltSpeed: TGroupBox;
    edAltTimeBegin: TMaskEdit;
    txAltUp: TLabel;
    txAltDown: TLabel;
    txMinutes1: TLabel;
    txTo: TLabel;
    txKbs3: TLabel;
    txKbs4: TLabel;
    txMinutes: TLabel;
    txMB: TLabel;
    txCacheSize: TLabel;
    Page: TPageControl;
    edCacheSize: TSpinEdit;
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
    procedure cbAutoAltClick(Sender: TObject);
    procedure cbBlocklistClick(Sender: TObject);
    procedure cbIdleSeedLimitClick(Sender: TObject);
    procedure cbIncompleteDirClick(Sender: TObject);
    procedure cbMaxDownClick(Sender: TObject);
    procedure cbMaxUpClick(Sender: TObject);
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

procedure TDaemonOptionsForm.cbAutoAltClick(Sender: TObject);
var
  i: integer;
begin
  edAltTimeBegin.Enabled:=cbAutoAlt.Checked;
  edAltTimeEnd.Enabled:=cbAutoAlt.Checked;
  txFrom.Enabled:=cbAutoAlt.Checked;
  txTo.Enabled:=cbAutoAlt.Checked;
  txDays.Enabled:=cbAutoAlt.Checked;
  for i:=1 to 7 do
    gbAltSpeed.FindChildControl(Format('cbDay%d', [i])).Enabled:=cbAutoAlt.Checked;
end;

procedure TDaemonOptionsForm.cbBlocklistClick(Sender: TObject);
begin
  if not edBlocklistURL.Visible then
    exit;
  edBlocklistURL.Enabled:=cbBlocklist.Checked;
  if edBlocklistURL.Enabled then
    edBlocklistURL.Color:=clWindow
  else
    edBlocklistURL.ParentColor:=True;
end;

procedure TDaemonOptionsForm.cbIdleSeedLimitClick(Sender: TObject);
begin
  edIdleSeedLimit.Enabled:=cbIdleSeedLimit.Checked;
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
  edBlocklistURL.Text:=Trim(edBlocklistURL.Text);
  if cbBlocklist.Checked and edBlocklistURL.Visible and (edBlocklistURL.Text = '') then begin
    Page.ActivePage:=tabNetwork;
    edBlocklistURL.SetFocus;
    MessageDlg(SNoBlocklistURL, mtError, [mbOK], 0);
    exit;
  end;
  if cbAutoAlt.Checked then begin
     if StrToTimeDef(edAltTimeBegin.Text, -1) < 0 then begin
       Page.ActivePage:=tabBandwidth;
       edAltTimeBegin.SetFocus;
       MessageDlg(SInvalidTime, mtError, [mbOK], 0);
       exit;
     end;
     if StrToTimeDef(edAltTimeEnd.Text, -1) < 0 then begin
       Page.ActivePage:=tabBandwidth;
       edAltTimeEnd.SetFocus;
       MessageDlg(SInvalidTime, mtError, [mbOK], 0);
       exit;
     end;
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

procedure TDaemonOptionsForm.cbRandomPortClick(Sender: TObject);
begin
  edPort.Enabled:=not cbRandomPort.Checked;
end;

procedure TDaemonOptionsForm.cbSeedRatioClick(Sender: TObject);
begin
  edSeedRatio.Enabled:=cbSeedRatio.Checked;
end;

procedure TDaemonOptionsForm.FormCreate(Sender: TObject);
var
  i, j, x, wd: integer;
  cb: TCheckBox;
begin
  Page.ActivePageIndex:=0;
  cbEncryption.Items.Add(sEncryptionDisabled);
  cbEncryption.Items.Add(sEncryptionEnabled);
  cbEncryption.Items.Add(sEncryptionRequired);
  Buttons.OKButton.ModalResult:=mrNone;
  Buttons.OKButton.OnClick:=@btOKClick;

  x:=edAltTimeBegin.Left;
  wd:=(gbAltSpeed.ClientWidth - x - BorderWidth) div 7;
  for i:=1 to 7 do begin
    cb:=TCheckBox.Create(gbAltSpeed);
    cb.Parent:=gbAltSpeed;
    j:=i + 1;
    if j > 7 then
      Dec(j, 7);
    cb.Caption:=SysToUTF8(ShortDayNames[j]);
    cb.Name:=Format('cbDay%d', [j]);
    cb.Left:=x;
    cb.Top:=txDays.Top - (cb.Height - txDays.Height) div 2;
    Inc(x, wd);
  end;
end;

initialization
  {$I daemonoptions.lrs}

end.

