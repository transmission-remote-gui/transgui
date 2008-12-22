unit DaemonOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin;

type

  { TDaemonOptionsForm }

  TDaemonOptionsForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbMaxDown: TCheckBox;
    cbMaxUp: TCheckBox;
    cbEncryption: TComboBox;
    cbPortForwarding: TCheckBox;
    cbPEX: TCheckBox;
    edDownloadDir: TEdit;
    edMaxDown: TSpinEdit;
    edMaxUp: TSpinEdit;
    gbBandwidth: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    txEncryption: TLabel;
    txPort: TLabel;
    edPort: TSpinEdit;
    txDownloadDir: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

initialization
  {$I daemonoptions.lrs}

end.

