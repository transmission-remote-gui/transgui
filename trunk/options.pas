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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Spin, Buttons, ButtonPanel, BaseForm,
  ConnOptions;

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
    cbCalcAvg: TCheckBox;
    cbRegExt: TCheckBox;
    cbRegMagnet: TCheckBox;
    edIntfScale: TSpinEdit;
    edCheckVersionDays: TSpinEdit;
    edRefreshInterval: TSpinEdit;
    edRefreshIntervalMin: TSpinEdit;
    gbTray: TGroupBox;
    gbNewTorrent: TGroupBox;
    gbData: TGroupBox;
    gbSysInt: TGroupBox;
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
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FLangList: TStringList;
{$ifdef mswindows}
    FRegExt: boolean;
    FRegMagnet: boolean;
{$endif mswindows}

    procedure FillLanguageItems;
  public
    ConnForm: TConnOptionsForm;
  end;

implementation

uses
  {$ifdef mswindows}
  registry,
  {$endif mswindows}
  main, utils, ResTranslator;

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  i: integer;
  pg: TTabSheet;
{$ifdef mswindows}
  reg: TRegistry;
  s: string;
{$endif mswindows}
begin
  cbRegExt.Caption:=Format(cbRegExt.Caption, [AppName]);
  cbRegMagnet.Caption:=Format(cbRegMagnet.Caption, [AppName]);

  ConnForm:=TConnOptionsForm.Create(Self);
  while ConnForm.Page.ControlCount > 0 do begin
    pg:=ConnForm.Page.Pages[0];
    pg.Parent:=Page;
    pg.TabVisible:=True;
  end;
  ConnForm.Page.Free;
  ConnForm.Page:=Page;

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

{$ifdef mswindows}
  gbSysInt.Visible:=True;
  reg:=TRegistry.Create;
  try
    if reg.OpenKeyReadOnly('Software\Classes\.torrent') then begin
      if reg.ReadString('') = AppName then begin
        reg.CloseKey;
        if reg.OpenKeyReadOnly(Format('Software\Classes\%s\shell\open\command', [AppName])) then begin
          s:=reg.ReadString('');
          FRegExt:=CompareFilePath(s, Format('"%s" "%%1"', [ParamStr(0)])) = 0;
        end;
      end;
    end;
    reg.CloseKey;
    if reg.OpenKeyReadOnly('Software\Classes\Magnet\shell\open\command') then begin
      s:=reg.ReadString('');
      FRegMagnet:=CompareFilePath(s, Format('"%s" "%%1"', [ParamStr(0)])) = 0;
    end;
  finally
    reg.Free;
  end;
  cbRegExt.Checked:=FRegExt;
  cbRegMagnet.Checked:=FRegMagnet;
{$endif mswindows}
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

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  ConnForm.FormShow(nil);
  if ConnForm.edHost.Text = '' then begin
    tabGeneral.Hide;
    ActiveControl:=ConnForm.edHost;
  end;
end;

procedure TOptionsForm.OKButtonClick(Sender: TObject);
var
  s: string;
  restart: boolean;
{$ifdef mswindows}
  reg: TRegistry;
{$endif mswindows}
begin
  ConnForm.ModalResult:=mrNone;
  ConnForm.btOKClick(nil);
  if ConnForm.ModalResult = mrNone then
    exit;
  restart:=False;
  if cbLanguage.Text <> FTranslationLanguage then begin
    if cbLanguage.Text = 'English' then
      s:='-'
    else
      s:=FLangList.Values[cbLanguage.Text];
    Ini.WriteString('Interface', 'TranslationFile', s);
    restart:=True;
  end;

{$ifdef mswindows}
  reg:=TRegistry.Create;
  try
    if cbRegExt.Checked <> FRegExt then
      if cbRegExt.Checked then begin
        if reg.OpenKey('Software\Classes\.torrent', True) then begin
          reg.WriteString('', AppName);
          reg.CloseKey;
          if reg.OpenKey(Format('Software\Classes\%s\DefaultIcon', [AppName]), True) then begin
            reg.WriteString('', Format('"%s",0', [ParamStr(0)]));
            reg.CloseKey;
            if reg.OpenKey(Format('Software\Classes\%s\shell\open\command', [AppName]), True) then begin
              reg.WriteString('', Format('"%s" "%%1"', [ParamStr(0)]));
              reg.CloseKey;
            end;
          end;
        end;
      end
      else begin
        if reg.OpenKey('Software\Classes\.torrent', False) then begin
          reg.DeleteValue('');
          reg.CloseKey;
        end;
        s:=Format('Software\Classes\%s', [AppName]);
        reg.DeleteKey(s + '\DefaultIcon');
        reg.DeleteKey(s + '\shell\open\command');
        reg.DeleteKey(s + '\shell\open');
        reg.DeleteKey(s + '\shell');
        reg.DeleteKey(s);
      end;

    if cbRegMagnet.Checked <> FRegMagnet then
      if cbRegMagnet.Checked then begin
        if reg.OpenKey('Software\Classes\Magnet', True) then begin
          if not reg.ValueExists('') then
            reg.WriteString('', 'Magnet URI');
          reg.WriteString('Content Type', 'application/x-magnet');
          reg.WriteString('URL Protocol', '');
          reg.CloseKey;
          if reg.OpenKey('Software\Classes\Magnet\DefaultIcon', True) then begin
            reg.WriteString('', Format('"%s",0', [ParamStr(0)]));
            reg.CloseKey;
            if reg.OpenKey('Software\Classes\Magnet\shell', True) then begin
              reg.WriteString('', 'open');
              reg.CloseKey;
              if reg.OpenKey('Software\Classes\Magnet\shell\open\command', True) then begin
                reg.WriteString('', Format('"%s" "%%1"', [ParamStr(0)]));
                reg.CloseKey;
              end;
            end;
          end;
        end;
      end
      else begin
        reg.DeleteKey('Software\Classes\Magnet\DefaultIcon');
        reg.DeleteKey('Software\Classes\Magnet\shell\open\command');
      end;
  finally
    reg.Free;
  end;
{$endif mswindows}

  if edIntfScale.Value <> IntfScale then
    restart:=True;

  if restart then
    MessageDlg(sRestartRequired, mtInformation, [mbOk], 0);
  ModalResult:=mrOk;
end;

initialization
  {$I options.lrs}

end.

