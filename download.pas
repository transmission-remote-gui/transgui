{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2012 by Yury Sidorov.

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

unit download;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  httpsend, synsock, ExtCtrls, BaseForm;

resourcestring
  SDownloadProgress = '%s of %s downloaded';
  SDownloadProgress2 = '%s downloaded';

type
  TDownloadThread = class;

  { TDownloadForm }

  TDownloadForm = class(TBaseForm)
    btCancel: TButton;
    UpdateTimer: TTimer;
    txPercent: TLabel;
    txBytes: TLabel;
    txFileName: TLabel;
    pbDownload: TProgressBar;
    procedure btCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FThread: TDownloadThread;
    FTotalSize: Int64;
    FDownloaded: Int64;
    FError: string;

    procedure UpdateStatus(Data: PtrInt);
  public
    { public declarations }
  end; 

  { TDownloadThread }

  TDownloadThread = class(TThread)
  private
    FHttp: THTTPSend;
    FForm: TDownloadForm;
    FUrl: string;
    FDestFileName: string;
    FOut: TFileStream;

    procedure DoMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
    procedure WriteToFile;
  protected
    procedure Execute; override;
  end;

function DownloadFile(const URL, DestFolder: string; const DestFileName: string = ''): boolean;

implementation

uses Main, rpc;

function DownloadFile(const URL, DestFolder: string; const DestFileName: string): boolean;
var
  s: string;
begin
  with TDownloadForm.Create(Application) do
  try
    s:=ExtractFileName(StringReplace(URL, '/', DirectorySeparator, [rfReplaceAll]));
    txFileName.Caption:=s;
    if DestFileName <> '' then
      s:=DestFileName;
    FThread.FUrl:=URL;
    FThread.FDestFileName:=IncludeTrailingPathDelimiter(DestFolder) + s;
    FThread.Resume;
    Result:=ShowModal = mrOk;
  finally
    Free;
  end;
end;

{ TDownloadThread }

procedure TDownloadThread.DoMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
begin
  if Terminated then begin
    FHttp.Abort;
    exit;
  end;

  if FHttp.DownloadSize <> 0 then begin
    FForm.FTotalSize:=FHttp.DownloadSize;
    Inc(FForm.FDownloaded, Len);
    WriteToFile;
  end;
end;

procedure TDownloadThread.WriteToFile;
begin
  if FOut = nil then
    FOut:=TFileStream.Create(FDestFileName, fmCreate);

  FHttp.Document.Position:=0;
  FOut.CopyFrom(FHttp.Document, FHttp.Document.Size);
  FHttp.Document.Clear;
end;

procedure TDownloadThread.Execute;
var
  res: PtrInt;
begin
  res:=1;
  try
    FHttp:=THTTPSend.Create;
    try
      if RpcObj.Http.ProxyHost <> '' then begin
        FHttp.ProxyHost:=RpcObj.Http.ProxyHost;
        FHttp.ProxyPort:=RpcObj.Http.ProxyPort;
        FHttp.ProxyUser:=RpcObj.Http.ProxyUser;
        FHttp.ProxyPass:=RpcObj.Http.ProxyPass;
      end;
      FHttp.Sock.OnMonitor:=@DoMonitor;
      if FHttp.HTTPMethod('GET', FUrl) then begin
        FForm.FDownloaded:=FHttp.DownloadSize;
        WriteToFile;
        res:=2;
      end
      else
        if not Terminated then
          FForm.FError:=FHttp.Sock.LastErrorDesc;
    finally
      FHttp.Free;
    end;
  except
    FForm.FError:=Exception(ExceptObject).Message;
  end;
  FOut.Free;
  if res = 1 then
    DeleteFile(FDestFileName);
  Application.QueueAsyncCall(@FForm.UpdateStatus, res);
  FForm.FThread:=nil;
end;

{ TDownloadForm }

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  FThread:=TDownloadThread.Create(True);
  FThread.FreeOnTerminate:=True;
  FThread.FForm:=Self;
  UpdateTimerTimer(nil);
end;

procedure TDownloadForm.btCancelClick(Sender: TObject);
begin
  btCancel.Enabled:=False;
  FThread.Terminate;
end;

procedure TDownloadForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FThread <> nil then begin
    CloseAction:=caNone;
    btCancel.Click;
  end;
end;

procedure TDownloadForm.FormDestroy(Sender: TObject);
begin
end;

procedure TDownloadForm.FormResize(Sender: TObject);
begin
  btCancel.Left:=(ClientWidth - btCancel.Width) div 2;
end;

procedure TDownloadForm.FormShow(Sender: TObject);
begin
  FormResize(nil);
end;

procedure TDownloadForm.UpdateTimerTimer(Sender: TObject);
begin
  if FTotalSize <> 0 then begin
    pbDownload.Max:=FTotalSize;
    pbDownload.Position:=FDownloaded;
    txPercent.Caption:=Format('%.1f%%', [FDownloaded*100/FTotalSize]);
    txBytes.Caption:=Format(SDownloadProgress, [GetHumanSize(FDownloaded), GetHumanSize(FTotalSize)]);
    txPercent.Show;
  end
  else begin
    txBytes.Caption:=Format(SDownloadProgress2, [GetHumanSize(FDownloaded)]);
    txPercent.Hide;
  end;
end;

procedure TDownloadForm.UpdateStatus(Data: PtrInt);
begin
  if Data <> 0 then begin
    if Data = 2 then
      ModalResult:=mrOk
    else begin
      if FError <> '' then
        MessageDlg(FError, mtError, [mbOK], 0);
      ModalResult:=mrCancel;
    end;
    exit;
  end;
end;

initialization
  {$I download.lrs}

end.

