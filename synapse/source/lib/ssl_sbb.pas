{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.006 |
|==============================================================================|
| Content: SSL support for SecureBlackBox 10+                                  |
|==============================================================================|
| Copyright (c)1999-2005, Lukas Gebauer                                        |
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
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2005.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Allen Drennan (adrennan@wiredred.com)                                      |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SSL plugin for Eldos SecureBlackBox)

For handling keys and certificates you can use this properties:
@link(TCustomSSL.CertCAFile), @link(TCustomSSL.CertCA),
@link(TCustomSSL.TrustCertificateFile), @link(TCustomSSL.TrustCertificate),
@link(TCustomSSL.PrivateKeyFile), @link(TCustomSSL.PrivateKey),
@link(TCustomSSL.CertificateFile), @link(TCustomSSL.Certificate),
@link(TCustomSSL.PFXFile). For usage of this properties and for possible formats
of keys and certificates refer to SecureBlackBox documentation.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ssl_sbb;

interface

uses
  SysUtils, Classes, Windows, blcksock, synsock, synautil, synacode,
  SBSSLClient, SBSSLServer, SBSSLConstants, SBX509, SBWinCertStorage,
  SBCustomCertStorage, SBTypes, SBUtils, SBConstants, SBSessionPool;

type
  {:@abstract(class implementing SecureBlackbox SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLSBB = class(TCustomSSL)
  protected
    FServer: Boolean;
    FElSecureClient: TElSSLClient;
    FElSecureServer: TElSSLServer;
    FElCertStorage: TElMemoryCertStorage;
    FElX509Certificate: TElX509Certificate;
    FElX509CACertificate: TElX509Certificate;
    FCipherSuites: TBits;
  private
    FAcceptThread: THandle;
    FRecvBuffer: AnsiString;
    FRecvBuffers: AnsiString;
    FRecvDecodedBuffers: AnsiString;
    FNoRecv: Integer;
  private
    function  GetCipherSuite: Integer;
    function  FileToString(const lFile: string): AnsiString;
    function  Prepare(Server: Boolean): Boolean;
    procedure Reset;

    procedure OnCertificateValidate(Sender: TObject; X509Certificate: TElX509Certificate;
      var Validate: TSBBoolean);
    procedure OnData(Sender: TObject; Buffer: Pointer; Size: LongInt);
    procedure OnError(Sender: TObject; ErrorCode: Integer; Fatal: Boolean;
      Remote: Boolean);
    procedure OnReceive(Sender: TObject; Buffer: Pointer; MaxSize: LongInt;
      out Written: LongInt);
    procedure OnSend(Sender: TObject; Buffer: Pointer; Size: LongInt);
  public
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: string; override;
    {:See @inherited}
    function LibName: string; override;
    {:See @inherited and @link(ssl_sbb) for more details.}
    function Connect: Boolean; override;
    {:See @inherited and @link(ssl_sbb) for more details.}
    function Accept: Boolean; override;
    {:See @inherited}
    function Shutdown: Boolean; override;
    {:See @inherited}
    function BiShutdown: Boolean; override;
    {:See @inherited}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function WaitingData: Integer; override;
    {:See @inherited}
    function GetSSLVersion: string; override;
    {:See @inherited}
    function GetPeerSubject: string; override;
    {:See @inherited}
    function GetPeerIssuer: string; override;
    {:See @inherited}
    function GetPeerName: string; override;
    {:See @inherited}
    function GetPeerFingerprint: string; override;
    {:See @inherited}
    function GetCertInfo: string; override;
  published
    property ElSecureClient: TElSSLClient read FElSecureClient write FElSecureClient;
    property ElSecureServer: TElSSLServer read FElSecureServer write FElSecureServer;
    property CipherSuites: TBits read FCipherSuites write FCipherSuites;
    property CipherSuite: Integer read GetCipherSuite;
  end;

implementation

const
  DEFAULT_RECV_BUFFER = 32768;

{ TSSLSBB }

function TSSLSBB.Accept: Boolean;
var
  lResult: Integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(True) then
  begin
    FAcceptThread := GetCurrentThreadId;
    FElSecureServer.Open;
    // reset
    FRecvBuffers := '';
    FRecvDecodedBuffers := '';
    // wait for open or error
    while (not FElSecureServer.Active) and
      (FLastError = 0) do
    begin
      // data available?
      if FRecvBuffers <> '' then
      begin
        while FRecvBuffers <> '' do
          FElSecureServer.DataAvailable;
      end
      else
      begin
        // socket recv - ntohs(FSocket.RemoteSin.sin_port)
        lResult := Recv(FSocket.Socket, @FRecvBuffer[1], Length(FRecvBuffer),
          0);
        if lResult = SOCKET_ERROR then
        begin
          FLastErrorDesc := '';
          FLastError := WSAGetLastError;
        end
        else
        begin
          if lResult > 0 then
            FRecvBuffers := FRecvBuffers + Copy(FRecvBuffer, 1, lResult)
          else
            Break;
        end;
      end;
    end;
    if FLastError <> 0 then
      Exit;
    FSSLEnabled := FElSecureServer.Active;
    Result := FSSLEnabled;
  end;
end;

function TSSLSBB.BiShutdown: Boolean;
begin
  Reset;
  Result := True;
end;

function TSSLSBB.Connect: Boolean;
var
  lResult: Integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(False) then
  begin
    FElSecureClient.Open;
    // reset
    FRecvBuffers := '';
    FRecvDecodedBuffers := '';
    // wait for open or error
    while (not FElSecureClient.Active) and
      (FLastError = 0) do
    begin
      // data available?
      if FRecvBuffers <> '' then
      begin
        while FRecvBuffers <> '' do
          FElSecureClient.DataAvailable;
      end
      else
      begin
        // socket recv
        lResult := synsock.Recv(FSocket.Socket, @FRecvBuffer[1],
          Length(FRecvBuffer), MSG_NOSIGNAL);
        if lResult = SOCKET_ERROR then
        begin
          FLastErrorDesc := '';
          FLastError := WSAGetLastError;
        end
        else
        begin
          if lResult > 0 then
            FRecvBuffers := FRecvBuffers + Copy(FRecvBuffer, 1, lResult)
          else
            Break;
        end;
      end;
    end;
    if FLastError <> 0 then
      Exit;
    FSSLEnabled := FElSecureClient.Active;
    Result := FSSLEnabled;
  end;
end;

{ inherited }
constructor TSSLSBB.Create(const Value: TTCPBlockSocket);
var
  loop1: Integer;
begin
  inherited Create(Value);
  FServer := False;
  FElSecureClient := nil;
  FElSecureServer := nil;
  FElCertStorage := nil;
  FElX509Certificate := nil;
  FElX509CACertificate := nil;
  SetLength(FRecvBuffer, DEFAULT_RECV_BUFFER);
  FRecvBuffers := '';
  FRecvDecodedBuffers := '';
  FNoRecv := 0;
  FCipherSuites := TBits.Create;
  if FCipherSuites <> nil then
  begin
    FCipherSuites.Size := SB_SUITE_LAST + 1;
    // disable exotic suites
    for loop1 := SB_SUITE_FIRST to SB_SUITE_LAST do
      FCipherSuites[loop1] := False;
    // enable common suites
    for loop1 := SB_SUITE_FIRST to SB_SUITE_DH_ANON_DES_SHA_EXPORT do
      FCipherSuites[loop1] := True;
  end;
end;

destructor TSSLSBB.Destroy;
begin
  Reset;
  inherited Destroy;
  if FCipherSuites <> nil then
    FreeAndNil(FCipherSuites);
end;

function TSSLSBB.FileToString(const lFile: string): AnsiString;
var
  lStream: TStream;
begin
  Result := '';
  lStream := TFileStream.Create(lFile, fmOpenRead or fmShareDenyWrite);
  try
    if lStream.Size > 0 then
    begin
      SetLength(Result, lStream.Size);
      lStream.Position := 0;
      lStream.ReadBuffer(PAnsiChar(Result)^, lStream.Size);
    end;
  finally
    lStream.Free;
  end;
end;

function TSSLSBB.GetCertInfo: string;
begin
  Result := '';
  // if FServer then
  // must return a text representation of the ASN of the client certificate
  // else
  // must return a text representation of the ASN of the server certificate
end;

function TSSLSBB.GetCipherSuite: Integer;
begin
  if FServer then
    Result := FElSecureServer.CipherSuite
  else
    Result := FElSecureClient.CipherSuite;
end;

function TSSLSBB.GetPeerFingerprint: string;
begin
  Result := '';
  // if FServer then
  // must return a unique hash string of the client certificate
  // else
  // must return a unique hash string of the server certificate
end;

function TSSLSBB.GetPeerIssuer: string;
begin
  Result := '';
  // if FServer then
  // must return issuer of the client certificate
  // else
  // must return issuer of the server certificate
end;

function TSSLSBB.GetPeerName: string;
begin
  Result := '';
  // if FServer then
  // must return commonname of the client certificate
  // else
  // must return commonname of the server certificate
end;

function TSSLSBB.GetPeerSubject: string;
begin
  Result := '';
  // if FServer then
  // must return subject of the client certificate
  // else
  // must return subject of the server certificate
end;

function TSSLSBB.GetSSLVersion: string;
begin
  Result := 'SSLv3 or TLSv1';
end;

function TSSLSBB.LibName: string;
begin
  Result := 'ssl_sbb';
end;

function TSSLSBB.LibVersion: string;
begin
  Result := 'SecureBlackBox 10+';
end;

// on certificate validate
procedure TSSLSBB.OnCertificateValidate(Sender: TObject;
  X509Certificate: TElX509Certificate; var Validate: TSBBoolean);
begin
  Validate := True;
end;

// on data
procedure TSSLSBB.OnData(Sender: TObject; Buffer: Pointer; Size: LongInt);
var
  lString: AnsiString;
begin
  SetLength(lString, Size);
  Move(Buffer^, lString[1], Size);
  FRecvDecodedBuffers := FRecvDecodedBuffers + lString;
end;

// on error
procedure TSSLSBB.OnError(Sender: TObject; ErrorCode: Integer; Fatal: Boolean;
  Remote: Boolean);
begin
  FLastErrorDesc := '';
  FLastError := ErrorCode;
end;

// on receive
procedure TSSLSBB.OnReceive(Sender: TObject; Buffer: Pointer; MaxSize: LongInt;
  out Written: LongInt);
var
  lLength: Integer;
begin
  lLength := Length(FRecvBuffers);
  if lLength <= MaxSize then
    Written := lLength
  else
    Written := MaxSize;
  Move(FRecvBuffers[1], Buffer^, Written);
  Delete(FRecvBuffers, 1, Written);
end;

// on send
procedure TSSLSBB.OnSend(Sender: TObject; Buffer: Pointer; Size: LongInt);
var
  lResult: Integer;
begin
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Size > 0 then
  begin
    lResult := synsock.Send(FSocket.Socket, Buffer, Size, MSG_NOSIGNAL);
    if lResult = SOCKET_ERROR then
    begin
      FLastErrorDesc := '';
      FLastError := WSAGetLastError;
    end;
  end;
end;

function TSSLSBB.Prepare(Server: Boolean): Boolean;
var
  loop1: Integer;
  lStream: TMemoryStream;
  lCertificate, lPrivateKey, lCertCA: AnsiString;
begin
  Result := False;
  FServer := Server;
  // reset, if necessary
  Reset;
  // init, certificate
  if FCertificateFile <> '' then
    lCertificate := FileToString(FCertificateFile)
  else
    lCertificate := FCertificate;
  if FPrivateKeyFile <> '' then
    lPrivateKey := FileToString(FPrivateKeyFile)
  else
    lPrivateKey := FPrivateKey;
  if FCertCAFile <> '' then
    lCertCA := FileToString(FCertCAFile)
  else
    lCertCA := FCertCA;
  if (lCertificate <> '') and (lPrivateKey <> '') then
  begin
    FElCertStorage := TElMemoryCertStorage.Create(nil);
    if FElCertStorage <> nil then
      FElCertStorage.Clear;
    // apply ca certificate
    if lCertCA <> '' then
    begin
      FElX509CACertificate := TElX509Certificate.Create(nil);
      if FElX509CACertificate <> nil then
      begin
        with FElX509CACertificate do
        begin
          lStream := TMemoryStream.Create;
          try
            WriteStrToStream(lStream, lCertCA);
            lStream.Seek(0, soFromBeginning);
            LoadFromStream(lStream);
          finally
            lStream.Free;
          end;
        end;
        if FElCertStorage <> nil then
          FElCertStorage.Add(FElX509CACertificate);
      end;
    end;
    // apply certificate
    FElX509Certificate := TElX509Certificate.Create(nil);
    if FElX509Certificate <> nil then
    begin
      with FElX509Certificate do
      begin
        lStream := TMemoryStream.Create;
        try
          WriteStrToStream(lStream, lCertificate);
          lStream.Seek(0, soFromBeginning);
          LoadFromStream(lStream);
        finally
          lStream.Free;
        end;
        lStream := TMemoryStream.Create;
        try
          WriteStrToStream(lStream, lPrivateKey);
          lStream.Seek(0, soFromBeginning);
          LoadKeyFromStream(lStream);
        finally
          lStream.Free;
        end;
        if FElCertStorage <> nil then
          FElCertStorage.Add(FElX509Certificate);
      end;
    end;
  end;
  // init, as server
  if FServer then
  begin
    FElSecureServer := TElSSLServer.Create(nil);
    if FElSecureServer <> nil then
    begin
      // init, ciphers
      for loop1 := SB_SUITE_FIRST to SB_SUITE_LAST do
        FElSecureServer.CipherSuites[loop1] := FCipherSuites[loop1];
      FElSecureServer.Versions := [sbSSL2, sbSSL3, sbTLS1];
      FElSecureServer.ClientAuthentication := False;
      FElSecureServer.OnError := OnError;
      FElSecureServer.OnSend := OnSend;
      FElSecureServer.OnReceive := OnReceive;
      FElSecureServer.OnData := OnData;
      FElSecureServer.CertStorage := FElCertStorage;
      Result := True;
    end;
  end
  else
    // init, as client
  begin
    FElSecureClient := TElSSLClient.Create(nil);
    if FElSecureClient <> nil then
    begin
      // init, ciphers
      for loop1 := SB_SUITE_FIRST to SB_SUITE_LAST do
        FElSecureClient.CipherSuites[loop1] := FCipherSuites[loop1];
      FElSecureClient.Versions := [sbSSL3, sbTLS1];
      FElSecureClient.OnError := OnError;
      FElSecureClient.OnSend := OnSend;
      FElSecureClient.OnReceive := OnReceive;
      FElSecureClient.OnData := OnData;
      FElSecureClient.OnCertificateValidate := OnCertificateValidate;
      FElSecureClient.CertStorage := FElCertStorage;
      Result := True;
    end;
  end;
end;

function TSSLSBB.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  lLength: Integer;
begin
  // recv waiting, if necessary
  if FRecvDecodedBuffers = '' then
    WaitingData;
  // received
  lLength := Length(FRecvDecodedBuffers);
  if lLength <= Len then
    Result := lLength
  else
    Result := Len;
  if Result > 0 then
  begin
    Move(FRecvDecodedBuffers[1], Buffer^, Result);
    Delete(FRecvDecodedBuffers, 1, Result);
  end;
end;

procedure TSSLSBB.Reset;
begin
  if FElSecureServer <> nil then
    FreeAndNil(FElSecureServer);
  if FElSecureClient <> nil then
    FreeAndNil(FElSecureClient);
  if FElX509Certificate <> nil then
    FreeAndNil(FElX509Certificate);
  if FElX509CACertificate <> nil then
    FreeAndNil(FElX509CACertificate);
  if FElCertStorage <> nil then
    FreeAndNil(FElCertStorage);
  FSSLEnabled := False;
end;

function TSSLSBB.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  if FServer then
    FElSecureServer.SendData(Buffer, Len)
  else
    FElSecureClient.SendData(Buffer, Len);
  Result := Len;
end;

function TSSLSBB.Shutdown: Boolean;
begin
  Result := BiShutdown;
end;

function TSSLSBB.WaitingData: Integer;
var
  lResult: Integer;
begin
  Result := 0;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if FRecvBuffers <> '' then
  begin
    while FRecvBuffers <> '' do
      if FServer then
        FElSecureServer.DataAvailable
      else
        FElSecureClient.DataAvailable;
  end
  else
  begin
    // socket recv
    lResult := Recv(FSocket.Socket, @FRecvBuffer[1], Length(FRecvBuffer), 0);
    if lResult = SOCKET_ERROR then
    begin
      FLastErrorDesc := '';
      FLastError := WSAGetLastError;
    end
    else
    begin
      if lResult > 0 then
      begin
        FRecvBuffers := FRecvBuffers + Copy(FRecvBuffer, 1, lResult);
        FNoRecv := 0;
        // data available?
        if FRecvBuffers <> '' then
        begin
          while FRecvBuffers <> '' do
            if FServer then
              FElSecureServer.DataAvailable
            else
              FElSecureClient.DataAvailable;
        end;
      end
      else
        // workaround for dead ssl sockets
        // which can happen if an open succeeds but the connection is abruptly
        // terminated by the client peer
      begin
        if FServer then
        begin
          Inc(FNoRecv);
          if FNoRecv > 25 then
            CloseSocket(FSocket.Socket);
        end;
      end;
    end;
  end;
  // decoded buffers result
  Result := Length(FRecvDecodedBuffers);
end;

{==============================================================================}
initialization
  SSLImplementation := TSSLSBB;

finalization

end.
