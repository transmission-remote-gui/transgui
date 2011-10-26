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

unit About;

{$mode objfpc}{$H+}

interface

uses
  BaseForm, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, ButtonPanel;

type

  { TAboutForm }

  TAboutForm = class(TBaseForm)
    Bevel1: TBevel;
    Buttons: TButtonPanel;
    edLicense: TMemo;
    imgDonate: TImage;
    imgTransmission: TImage;
    imgSynapse: TImage;
    imgLazarus: TImage;
    txDonate: TLabel;
    txHomePage: TLabel;
    txAuthor: TLabel;
    txVersion: TLabel;
    txAppName: TLabel;
    Page: TPageControl;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure imgDonateClick(Sender: TObject);
    procedure imgLazarusClick(Sender: TObject);
    procedure imgSynapseClick(Sender: TObject);
    procedure txHomePageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses Main, utils;

{ TAboutForm }

procedure TAboutForm.imgSynapseClick(Sender: TObject);
begin
  AppBusy;
  OpenURL('http://synapse.ararat.cz');
  AppNormal;
end;

procedure TAboutForm.txHomePageClick(Sender: TObject);
begin
  AppBusy;
  OpenURL(txHomePage.Caption);
  AppNormal;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
{$ifdef lclcarbon}
var
  s: string;
{$endif lclcarbon}
begin
  txAppName.Font.Size:=Font.Size + 2;
  txHomePage.Font.Size:=Font.Size;
  BorderStyle:=bsSizeable;
  txAppName.Caption:=AppName;
  txVersion.Caption:=Format(txVersion.Caption, [AppVersion]);
  Page.ActivePageIndex:=0;
{$ifdef lclcarbon}
  s:=edLicense.Text;
  edLicense.Text:='';
  edLicense.HandleNeeded;
  edLicense.Text:=s;
{$endif lclcarbon}
end;

procedure TAboutForm.imgDonateClick(Sender: TObject);
begin
  AppBusy;
  OpenURL('https://www.plimus.com/jsp/buynow.jsp?contractId=2343710&paymentMethod=paypal');
  AppNormal;
end;

procedure TAboutForm.imgLazarusClick(Sender: TObject);
begin
  AppBusy;
  OpenURL('http://www.lazarus.freepascal.org');
  AppNormal;
end;

initialization
  {$I about.lrs}

end.

