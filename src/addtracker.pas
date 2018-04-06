unit AddTracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, ExtCtrls, BaseForm;

resourcestring
  STrackerProps = 'Tracker properties';
  SNoTracker = 'No tracker URL was specified.';

type

  { TAddTrackerForm }

  TAddTrackerForm = class(TBaseForm)
    Buttons: TButtonPanel;
    edTracker: TEdit;
    Panel1: TPanel;
    txTrackerURL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses main;

{ TAddTrackerForm }

procedure TAddTrackerForm.OKButtonClick(Sender: TObject);
begin
  edTracker.Text:=Trim(edTracker.Text);
  if edTracker.Text = '' then begin
    edTracker.SetFocus;
    MessageDlg(SNoTracker, mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TAddTrackerForm.FormCreate(Sender: TObject);
begin
  Buttons.OKButton.ModalResult:=mrNone;
  Buttons.OKButton.OnClick:=@OKButtonClick;
end;

initialization
  {$I addtracker.lrs}

end.

