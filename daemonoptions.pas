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

type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbEncryption: TComboBox;
    cbPortForwarding: TCheckBox;
    cbPEX: TCheckBox;
    edDownloadDir: TEdit;
    edMaxDown: TSpinEdit;
    edMaxUp: TSpinEdit;
    gbBandwidth: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edMaxPeers: TSpinEdit;
    txPeerLimit: TLabel;
    txEncryption: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txDownloadDir: TLabel;
    procedure cbMaxDownClick(Sender: TObject);
    procedure cbMaxUpClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{ TDaemonOptionsForm }

procedure TDaemonOptionsForm.cbMaxDownClick(Sender: TObject);
begin
  edMaxDown.Enabled:=cbMaxDown.Checked;
end;

procedure TDaemonOptionsForm.cbMaxUpClick(Sender: TObject);
begin
  edMaxUp.Enabled:=cbMaxUp.Checked;
end;

initialization
  {$I daemonoptions.lrs}

end.

