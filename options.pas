{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2012 by Yury Sidorov.

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

unit Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Spin, Buttons, ButtonPanel, BaseForm;

type

  { TOptionsForm }

  TOptionsForm = class(TBaseForm)
    Buttons: TButtonPanel;
    cbDeleteTorrentFile: TCheckBox;
    cbLanguage: TComboBox;
    cbLinksFromClipboard: TCheckBox;
    cbShowAddTorrentWindow: TCheckBox;
    cbTrayClose: TCheckBox;
    cbTrayIconAlways: TCheckBox;
    cbTrayMinimize: TCheckBox;
    cbCheckNewVersion: TCheckBox;
    edIntfScale: TSpinEdit;
    edCheckVersionDays: TSpinEdit;
    edRefreshInterval: TSpinEdit;
    edRefreshIntervalMin: TSpinEdit;
    gbTray: TGroupBox;
    gbNewTorrent: TGroupBox;
    gbRefresh: TGroupBox;
    txDays: TLabel;
    tabGeneral: TTabSheet;
    txPerc: TLabel;
    Page: TPageControl;
    tabAdvanced: TTabSheet;
    txLanguage: TLabel;
    txIntfScale: TLabel;
    txRefreshInterval: TLabel;
    txRefreshIntervalMin: TLabel;
    txSeconds: TLabel;
    txSeconds2: TLabel;
    procedure cbCheckNewVersionClick(Sender: TObject);
    procedure cbLanguageEnter(Sender: TObject);
    procedure cbLanguageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbLanguageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FLangList: TStringList;

    procedure FillLanguageItems;
  public
    { public declarations }
  end; 

implementation

uses main, utils, ResTranslator;

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Page.ActivePageIndex:=0;
  Buttons.OKButton.ModalResult:=mrNone;
  Buttons.OKButton.OnClick:=@OKButtonClick;

  cbLanguage.Items.Add(FTranslationLanguage);
  cbLanguage.ItemIndex:=0;
  i:=80*100 div (ScaleInt(100)*100 div IntfScale);
  i:=i - i mod 5;
  if i < 10 then
    i:=10;
  edIntfScale.MinValue:=i;
{$ifdef LCLgtk2}
  cbLanguage.OnDropDown:=@cbLanguageEnter;
  cbLanguage.OnMouseMove:=@cbLanguageMouseMove;
{$endif LCLgtk2}
end;

procedure TOptionsForm.cbLanguageEnter(Sender: TObject);
begin
  if not Assigned(FLangList) then
    FillLanguageItems;
end;

procedure TOptionsForm.cbCheckNewVersionClick(Sender: TObject);
begin
  edCheckVersionDays.Enabled:=cbCheckNewVersion.Checked;
end;

procedure TOptionsForm.cbLanguageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cbLanguageEnter(cbLanguage);
end;

procedure TOptionsForm.cbLanguageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  cbLanguageEnter(cbLanguage);
end;

procedure TOptionsForm.FillLanguageItems;
var
  i: integer;
begin
  AppBusy;
  cbLanguage.Items.BeginUpdate;
  try
    cbLanguage.Items.Clear;
    cbLanguage.Items.Add('English');
    FLangList := GetAvailableTranslations;
    FLangList.Sort;
    with FLangList do
      for i := 0 to Count - 1 do
        cbLanguage.Items.Add(Names[i]);
    with cbLanguage do
      ItemIndex:= Items.IndexOf(FTranslationLanguage);
  finally
    cbLanguage.Items.EndUpdate;
  end;
  AppNormal;
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
begin
  FLangList.Free;
end;

procedure TOptionsForm.OKButtonClick(Sender: TObject);
var
  s: string;
  restart: boolean;
begin
  restart:=False;
  if cbLanguage.Text <> FTranslationLanguage then begin
    if cbLanguage.Text = 'English' then
      s:='-'
    else
      s:=FLangList.Values[cbLanguage.Text];
    Ini.WriteString('Interface', 'TranslationFile', s);
    restart:=True;
  end;

  if edIntfScale.Value <> IntfScale then
    restart:=True;

  if restart then
    MessageDlg(sRestartRequired, mtInformation, [mbOk], 0);
  ModalResult:=mrOk;
end;

initialization
  {$I options.lrs}

end.

