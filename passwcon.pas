unit passwcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TPasswordConnect }

  TPasswordConnect = class(TForm)
    Buttons: TButtonPanel;
    passw: TEdit;
    lMsg: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SetText(form : string; msg : string);

  private
    { private declarations }
  public
    { public declarations }
    gPassw : string;
  end;

var
  PasswordConnect: TPasswordConnect;

implementation

uses main;

{ TPasswordConnect }

procedure TPasswordConnect.CancelButtonClick(Sender: TObject);
begin

end;

procedure TPasswordConnect.FormCreate(Sender: TObject);
begin

  Buttons.OKButton.ModalResult:= mrNone;
  bidiMode := GetBiDi;
  gPassw := '';
  passw.Text := '';
end;

procedure TPasswordConnect.OKButtonClick(Sender: TObject);
begin
  gPassw:= passw.Text;
  ModalResult:=mrOk;
end;


procedure TPasswordConnect.SetText(form : string; msg : string);
begin
  lMsg.Caption := msg;
  Caption := form;
end;


initialization
  {$I passwcon.lrs}

end.

