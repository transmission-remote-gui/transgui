{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008 by Yury Sidorov.

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

unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btOK: TButton;
    edLicense: TMemo;
    imgTransmission: TImage;
    imgSynapse: TImage;
    imgLazarus: TImage;
    txAuthor: TLabel;
    txVersion: TLabel;
    txAppName: TLabel;
    Page: TPageControl;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure imgSynapseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses Main;

{ TAboutForm }

procedure TAboutForm.imgSynapseClick(Sender: TObject);
begin

end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  txAppName.Caption:=AppName;
  txVersion.Caption:=Format(txVersion.Caption, [AppVersion]);
end;

initialization
  {$I about.lrs}

end.

