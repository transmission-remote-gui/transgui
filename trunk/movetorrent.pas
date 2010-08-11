unit MoveTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

resourcestring
  SNoTorrentDir = 'No torrent location was specified.';

type

  { TMoveTorrentForm }

  TMoveTorrentForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbMoveData: TCheckBox;
    edTorrentDir: TComboBox;
    txTorrentDir: TLabel;
    procedure btOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{ TMoveTorrentForm }

procedure TMoveTorrentForm.btOKClick(Sender: TObject);
begin
  edTorrentDir.Text:=Trim(edTorrentDir.Text);
  if edTorrentDir.Text = '' then begin
    edTorrentDir.SetFocus;
    MessageDlg(SNoTorrentDir, mtError, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

initialization
  {$I movetorrent.lrs}

end.

