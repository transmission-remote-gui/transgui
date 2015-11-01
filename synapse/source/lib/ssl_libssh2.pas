{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.000 |
|==============================================================================|
| Content: SSH support by LibSSH2                                              |
|==============================================================================|
| Copyright (c)1999-2013, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Alexey Suhinin.                |
| Portions created by Alexey Suhinin are Copyright (c)2012-2013.                |
| Portions created by Lukas Gebauer are Copyright (c)2013-2013.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//requires LibSSH2 libraries! http://libssh2.org

{:@abstract(SSH plugin for LibSSH2)

Requires libssh2.dll or libssh2.so. 
You can download binaries as part of the CURL project from 
http://curl.haxx.se/download.html

You need Pascal bindings for the library too! You can find one at:
 http://www.lazarus.freepascal.org/index.php/topic,15935.msg86465.html#msg86465

This plugin implements the client part only.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ssl_libssh2;

interface
 
uses
  SysUtils,
  blcksock, synsock,
  libssh2;
 
type
  {:@abstract(class implementing LibSSH2 SSH plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLLibSSH2 = class(TCustomSSL)
  protected
    FSession: PLIBSSH2_SESSION;
    FChannel: PLIBSSH2_CHANNEL;
    function SSHCheck(Value: integer): Boolean;
    function DeInit: Boolean;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited}
    function Connect: boolean; override;
    {:See @inherited}
    function Shutdown: boolean; override;
    {:See @inherited}
    function BiShutdown: boolean; override;
    {:See @inherited}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function WaitingData: Integer; override;
    {:See @inherited}
    function GetSSLVersion: string; override;
  published
  end;
 
implementation
 
{==============================================================================}
function TSSLLibSSH2.SSHCheck(Value: integer): Boolean;
var
  PLastError: PAnsiChar;
  ErrMsgLen: Integer;
begin
  Result := true;
  FLastError := 0;
  FLastErrorDesc := '';
  if Value<0 then
  begin
    FLastError := libssh2_session_last_error(FSession, PLastError, ErrMsglen, 0);
    FLastErrorDesc := PLastError;
    Result := false;
  end;
end;
 
 
function TSSLLibSSH2.DeInit: Boolean;
begin
  if Assigned(FChannel) then
  begin
    libssh2_channel_free(FChannel);
    FChannel := nil;
  end;
  if Assigned(FSession) then
  begin
    libssh2_session_disconnect(FSession,'Goodbye');
    libssh2_session_free(FSession);
    FSession := nil;
  end;
  FSSLEnabled := False;
  Result := true;
end;
 
constructor TSSLLibSSH2.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FSession := nil;
  FChannel := nil;
end;
 
destructor TSSLLibSSH2.Destroy;
begin
  DeInit;
  inherited Destroy;
end;
 
function TSSLLibSSH2.Connect: boolean;
begin
  Result := False;
  if SSLEnabled then DeInit;
  if (FSocket.Socket <> INVALID_SOCKET) and (FSocket.SSL.SSLType = LT_SSHv2) then
    begin
      FSession := libssh2_session_init();
      if not Assigned(FSession) then
      begin
        FLastError := -999;
        FLastErrorDesc := 'Cannot initialize SSH session';
        exit;
      end;
      if not SSHCheck(libssh2_session_startup(FSession, FSocket.Socket)) then
        exit;
      // Attempt private key authentication, then fall back to username/password but
      // do not forget original private key auth error. This avoids giving spurious errors like
      // Authentication failed (username/password)
      // instead of e.g.
      // Unable to extract public key from private key file: Method unimplemented in libgcrypt backend
      if FSocket.SSL.PrivateKeyFile<>'' then
        if (not SSHCheck(libssh2_userauth_publickey_fromfile(FSession, PChar(FSocket.SSL.Username), nil, PChar(FSocket.SSL.PrivateKeyFile), PChar(FSocket.SSL.KeyPassword))))
          and (libssh2_userauth_password(FSession, PChar(FSocket.SSL.Username), PChar(FSocket.SSL.Password))<0) then
            exit;
      FChannel := libssh2_channel_open_session(FSession);
      if not assigned(FChannel) then
      begin
//        SSHCheck(-1);
        FLastError:=-999;
        FLastErrorDesc := 'Cannot open session';
        exit;
      end;
      if not SSHCheck(libssh2_channel_request_pty(FChannel, 'vanilla')) then
        exit;
      if not SSHCheck(libssh2_channel_shell(FChannel)) then
        exit;
      FSSLEnabled := True;
      Result := True;
    end;
end;

function TSSLLibSSH2.LibName: String;
begin
  Result := 'ssl_libssh2';
end;
 
function TSSLLibSSH2.Shutdown: boolean;
begin
  Result := DeInit;
end;
 
 
function TSSLLibSSH2.BiShutdown: boolean;
begin
  Result := DeInit;
end;
 
function TSSLLibSSH2.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  Result:=libssh2_channel_write(FChannel, PAnsiChar(Buffer), Len);
  SSHCheck(Result);
end;

function TSSLLibSSH2.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  result:=libssh2_channel_read(FChannel, PAnsiChar(Buffer), Len);
  SSHCheck(Result);
end;
 
function TSSLLibSSH2.WaitingData: Integer;
begin
  if libssh2_poll_channel_read(FChannel, Result) <> 1 then
    Result := 0;
end;
 
function TSSLLibSSH2.GetSSLVersion: string;
begin
  Result := 'SSH2';
end;

function TSSLLibSSH2.LibVersion: String;
begin
  Result := libssh2_version(0);
end;

initialization
  if libssh2_init(0)=0 then
    SSLImplementation := TSSLLibSSH2;
 
finalization
  libssh2_exit;
 
end.
