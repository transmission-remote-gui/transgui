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

unit DaemonOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin;

resourcestring
 sPortTestSuccess = 'Incoming port tested sucessfully.';
 sPortTestFailed = 'Incoming port is closed. Check your firewall settings.';
 sEncryptionDisabled = 'Encryption disabled';
 sEncryptionEnabled = 'Encryption enabled';
 sEncryptionRequired = 'Encryption required';

type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    btTestPort: TButton;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbEncryption: TComboBox;
    cbPortForwarding: TCheckBox;
    cbPEX: TCheckBox;
    cbDHT: TCheckBox;
    cbRandomPort: TCheckBox;
    cbSeedRatio: TCheckBox;
    edDownloadDir: TEdit;
    edMaxDown: TSpinEdit;
    edMaxUp: TSpinEdit;
    edSeedRatio: TFloatSpinEdit;
    gbBandwidth: TGroupBox;
    gbGeneral: TGroupBox;
    txKbs1: TLabel;
    txKbs2: TLabel;
    edMaxPeers: TSpinEdit;
    txPeerLimit: TLabel;
    txEncryption: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txDownloadDir: TLabel;
    procedure btTestPortClick(Sender: TObject);
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
{$ifdef windows}
  gbGeneral.Caption:='';
{$endif windows}
  cbEncryption.Items.Add(sEncryptionDisabled);
  cbEncryption.Items.Add(sEncryptionEnabled);
  cbEncryption.Items.Add(sEncryptionRequired);
end;

initialization
  {$I daemonoptions.lrs}

end.

