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

unit AddLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

resourcestring
  SNoLink = 'No link was specified.';

type

  { TAddLinkForm }

  TAddLinkForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    edLink: TEdit;
    txLink: TLabel;
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses main;

{ TAddLinkForm }

procedure TAddLinkForm.btOKClick(Sender: TObject);
begin
  edLink.Text:=Trim(edLink.Text);
  if edLink.Text = '' then begin
    edLink.SetFocus;
    MessageDlg(SNoLink, mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

procedure TAddLinkForm.FormCreate(Sender: TObject);
begin
  Font.Size:=MainForm.Font.Size;
end;

initialization
  {$I addlink.lrs}

end.

