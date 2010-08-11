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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AddLinkForm: TAddLinkForm;

implementation

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

initialization
  {$I addlink.lrs}

end.

