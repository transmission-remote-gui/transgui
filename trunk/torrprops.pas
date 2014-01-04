{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2014 by Yury Sidorov.

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

unit TorrProps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ButtonPanel, ComCtrls, BaseForm;

type

  { TTorrPropsForm }

  TTorrPropsForm = class(TBaseForm)
    Buttons: TButtonPanel;
    cbIdleSeedLimit: TCheckBox;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbSeedRatio: TCheckBox;
    edIdleSeedLimit: TSpinEdit;
    edMaxUp: TSpinEdit;
    edPeerLimit: TSpinEdit;
    edSeedRatio: TFloatSpinEdit;
    edMaxDown: TSpinEdit;
    edTrackers: TMemo;
    txTrackers: TLabel;
    Page: TPageControl;
    tabGeneral: TTabSheet;
    tabAdvanced: TTabSheet;
    txKbs1: TLabel;
    txKbs2: TLabel;
    txMinutes: TLabel;
    txName: TLabel;
    txPeerLimit: TLabel;
    procedure cbIdleSeedLimitClick(Sender: TObject);
    procedure cbMaxDownClick(Sender: TObject);
    procedure cbMaxUpClick(Sender: TObject);
    procedure cbSeedRatioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses main;

{ TTorrPropsForm }

procedure TTorrPropsForm.cbMaxDownClick(Sender: TObject);
begin
  edMaxDown.Enabled:=cbMaxDown.Checked;
end;

procedure TTorrPropsForm.cbIdleSeedLimitClick(Sender: TObject);
begin
  edIdleSeedLimit.Enabled:=cbIdleSeedLimit.State = cbChecked;
end;

procedure TTorrPropsForm.cbMaxUpClick(Sender: TObject);
begin
  edMaxUp.Enabled:=cbMaxUp.Checked;
end;

procedure TTorrPropsForm.cbSeedRatioClick(Sender: TObject);
begin
  edSeedRatio.Enabled:=cbSeedRatio.State = cbChecked;
end;

procedure TTorrPropsForm.FormCreate(Sender: TObject);
begin
  Page.ActivePageIndex:=0;
end;

initialization
  {$I torrprops.lrs}

end.

