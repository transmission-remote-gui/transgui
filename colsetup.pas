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

unit ColSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, CheckLst, StdCtrls;

type

  { TColSetupForm }

  TColSetupForm = class(TForm)
    btUp: TButton;
    btDown: TButton;
    btOk: TButton;
    btCancel: TButton;
    lstColumns: TCheckListBox;
    procedure btDownClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure btUpClick(Sender: TObject);
    procedure lstColumnsClick(Sender: TObject);
  private
    procedure UpdateUI;
    procedure MoveItem(Delta: integer);
  public
    { public declarations }
  end; 

function SetupColumns(LV: TListView): boolean;

implementation

function SetupColumns(LV: TListView): boolean;
var
  i, j: integer;
begin
  with TColSetupForm.Create(Application) do
  try
    for i:=0 to LV.Columns.Count - 1 do
      with LV.Columns[i] do begin
        j:=lstColumns.Items.Add(Caption);
        lstColumns.Items.Objects[j]:=TObject(ptrint(ID));
        if Width = 0 then
          Visible:=False;
        lstColumns.Checked[j]:=Visible;
        if Visible or (Width <> 0) then
          Tag:=Width;
      end;
    UpdateUI;
    Result:=ShowModal = mrOk;
    if Result then begin
      LV.BeginUpdate;
      try
        for i:=0 to lstColumns.Items.Count - 1 do
          for j:=0 to LV.Columns.Count - 1 do
            with LV.Columns[j] do
              if ID = ptrint(lstColumns.Items.Objects[i]) then begin
                Index:=i;
                if not Visible and (Visible <> lstColumns.Checked[i]) then begin
                  Visible:=True;
                  if Width <> 0 then
                    Tag:=Width;
                  Width:=0;
                  if Tag < 32 then
                    Tag:=32;
                  Width:=Tag;
                end
                else
                  Visible:=lstColumns.Checked[i];
                ImageIndex:=0;
                ImageIndex:=-1;
                break;
              end;
      finally
        LV.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
  Application.ProcessMessages;
end;

{ TColSetupForm }

procedure TColSetupForm.btOkClick(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to lstColumns.Items.Count - 1 do
    if lstColumns.Checked[i] then begin
      ModalResult:=mrOk;
      exit;
    end;
  MessageDlg('At least single column must be visible.', mtError, [mbOK], 0);
end;

procedure TColSetupForm.btDownClick(Sender: TObject);
begin
  MoveItem(1);
end;

procedure TColSetupForm.btUpClick(Sender: TObject);
begin
  MoveItem(-1);
end;

procedure TColSetupForm.lstColumnsClick(Sender: TObject);
begin
  UpdateUI;
end;

procedure TColSetupForm.UpdateUI;
begin
  btUp.Enabled:=lstColumns.ItemIndex > 0;
  btDown.Enabled:=(lstColumns.ItemIndex >= 0) and (lstColumns.ItemIndex < lstColumns.Items.Count - 1);
end;

procedure TColSetupForm.MoveItem(Delta: integer);
var
  c: boolean;
  OldIdx: integer;
begin
  OldIdx:=lstColumns.ItemIndex;
  c:=lstColumns.Checked[OldIdx];
  lstColumns.Items.Move(OldIdx, OldIdx+Delta);
  lstColumns.Checked[OldIdx+Delta]:=c;
  lstColumns.ItemIndex:=OldIdx+Delta;
  UpdateUI;
end;

initialization
  {$I colsetup.lrs}

end.

