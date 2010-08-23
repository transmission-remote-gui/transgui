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

