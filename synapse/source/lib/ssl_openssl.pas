{==============================================================================|
| Project : Ararat Synapse                                       | 001.003.000 |
|==============================================================================|
| Content: SSL support by OpenSSL                                              |
|==============================================================================|
| Copyright (c)1999-2017, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2005-2017.                |
| Portions created by Petr Fejfar are Copyright (c)2011-2012.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//requires OpenSSL libraries!

{:@abstract(SSL plugin for OpenSSL)

Compatibility with OpenSSL versions:
0.9.6 should work, known mysterious crashing on FreePascal and Linux platform.
0.9.7 - 1.0.0 working fine.
1.1.0 should work, under testing.

OpenSSL libraries are loaded dynamicly - you not need OpenSSL librares even you
compile your application with this unit. SSL just not working when you not have
OpenSSL libraries.

This plugin have limited support for .NET too! Because is not possible to use
callbacks with CDECL calling convention under .NET, is not supported
key/certificate passwords and multithread locking. :-(

For handling keys and certificates you can use this properties:

@link(TCustomSSL.CertificateFile) for PEM or ASN1 DER (cer) format. @br
@link(TCustomSSL.Certificate) for ASN1 DER format only. @br
@link(TCustomSSL.PrivateKeyFile) for PEM or ASN1 DER (key) format. @br
@link(TCustomSSL.PrivateKey) for ASN1 DER format only. @br
@link(TCustomSSL.CertCAFile) for PEM CA certificate bundle. @br
@link(TCustomSSL.PFXFile) for PFX format. @br
@link(TCustomSSL.PFX) for PFX format from binary string. @br

This plugin is capable to create Ad-Hoc certificates. When you start SSL/TLS
server without explicitly assigned key and certificate, then this plugin create
Ad-Hoc key and certificate for each incomming connection by self. It slowdown
accepting of new connections!
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ssl_openssl;

interface

uses
  SysUtils, Classes,
  blcksock, synsock, synautil, synaip,
{$IFDEF CIL}
  System.Text,
{$ENDIF}
  ssl_openssl_lib;

type
  {:@abstract(class implementing OpenSSL SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLOpenSSL = class(TCustomSSL)
  protected
    FSsl: PSSL;
    Fctx: PSSL_CTX;
    function SSLCheck: Boolean;
    function SetSslKeys(server:Boolean): boolean;
    function Init(server:Boolean): Boolean;
    function DeInit: Boolean;
    function Prepare(server:Boolean): Boolean;
    procedure SetWaitError;
    function WaitForIO(Error: integer; StartTick: LongWord): Boolean;
    function LoadPFX(pfxdata: ansistring): Boolean;
    function ConfigureHostVerification: Boolean;
    function CreateSelfSignedCert(Host: string): Boolean; override;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
    function Connect: boolean; override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
    function Accept: boolean; override;
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
    {:See @inherited}
    function GetPeerSubject: string; override;
    {:See @inherited}
    function GetPeerSerialNo: integer; override; {pf}
    {:See @inherited}
    function GetPeerIssuer: string; override;
    {:See @inherited}
    function GetPeerName: string; override;
    {:See @inherited}
    function GetPeerNameHash: cardinal; override; {pf}
    {:See @inherited}
    function GetPeerFingerprint: string; override;
    {:See @inherited}
    function GetCertInfo: string; override;
    {:See @inherited}
    function GetCipherName: string; override;
    {:See @inherited}
    function GetCipherBits: integer; override;
    {:See @inherited}
    function GetCipherAlgBits: integer; override;
    {:See @inherited}
    function GetVerifyCert: integer; override;
  end;

implementation

{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
uses
  Windows;

type
  TWinCertContext = record
    EncodingType: DWORD;
    EncodedData: PByte;
    EncodedSize: DWORD;
    CertInfo: Pointer;
    CertStore: Pointer;
  end;
  PWinCertContext = ^TWinCertContext;
  TPAnsiCharArray = array[0..0] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;
  TWinCertEnhKeyUsage = record
    UsageIdentifierCount: DWORD;
    UsageIdentifiers: PPAnsiCharArray;
  end;
  PWinCertEnhKeyUsage = ^TWinCertEnhKeyUsage;

const
  CRYPT_E_NOT_FOUND = DWORD($80092004);
  X509_ASN_ENCODING = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;
  CERT_FIND_EXISTING = $000D0000;
  CERT_STORE_PROV_SYSTEM_A = 9;
  CERT_STORE_READONLY_FLAG = $00008000;
  CERT_SYSTEM_STORE_CURRENT_USER = $00010000;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;
  ANY_ENHANCED_KEY_USAGE_OID = '2.5.29.37.0';
  SERVER_AUTH_OID = '1.3.6.1.5.5.7.3.1';
  X509_R_CERT_ALREADY_IN_HASH_TABLE = 101;

threadvar
  WindowsVerifyError: string;
  WindowsCurrentUserDisallowedStore: Pointer;
  WindowsLocalMachineDisallowedStore: Pointer;

function WinCertOpenStore(Provider: Pointer; EncodingType: DWORD;
  CryptProvider: PtrUInt; Flags: DWORD; StoreName: Pointer): Pointer; stdcall;
  external 'crypt32.dll' name 'CertOpenStore';
function WinCertEnumCertificatesInStore(Store: Pointer;
  Previous: PWinCertContext): PWinCertContext; stdcall;
  external 'crypt32.dll' name 'CertEnumCertificatesInStore';
function WinCertCreateCertificateContext(EncodingType: DWORD;
  EncodedData: PByte; EncodedSize: DWORD): PWinCertContext; stdcall;
  external 'crypt32.dll' name 'CertCreateCertificateContext';
function WinCertFindCertificateInStore(Store: Pointer; EncodingType: DWORD;
  FindFlags: DWORD; FindType: DWORD; FindParameter: Pointer;
  Previous: PWinCertContext): PWinCertContext; stdcall;
  external 'crypt32.dll' name 'CertFindCertificateInStore';
function WinCertGetEnhancedKeyUsage(CertContext: PWinCertContext; Flags: DWORD;
  Usage: PWinCertEnhKeyUsage; var UsageSize: DWORD): LongBool; stdcall;
  external 'crypt32.dll' name 'CertGetEnhancedKeyUsage';
function WinCertFreeCertificateContext(CertContext: PWinCertContext): LongBool;
  stdcall; external 'crypt32.dll' name 'CertFreeCertificateContext';
function WinCertCloseStore(Store: Pointer; Flags: DWORD): LongBool;
  stdcall; external 'crypt32.dll' name 'CertCloseStore';

function OpenWindowsCertificateStore(const StoreName: PAnsiChar;
  Location: DWORD): Pointer;
begin
  Result := WinCertOpenStore(Pointer(PtrUInt(CERT_STORE_PROV_SYSTEM_A)), 0, 0,
    Location or CERT_STORE_READONLY_FLAG, StoreName);
end;

function OpenWindowsDisallowedStores(out ErrorDescription: string): Boolean;
begin
  Result := False;
  ErrorDescription := '';
  WindowsVerifyError := '';
  WindowsCurrentUserDisallowedStore :=
    OpenWindowsCertificateStore('Disallowed',
      CERT_SYSTEM_STORE_CURRENT_USER);
  if WindowsCurrentUserDisallowedStore = nil then
  begin
    ErrorDescription :=
      'Unable to open the current-user Windows Disallowed certificate store.';
    Exit;
  end;
  WindowsLocalMachineDisallowedStore :=
    OpenWindowsCertificateStore('Disallowed',
      CERT_SYSTEM_STORE_LOCAL_MACHINE);
  if WindowsLocalMachineDisallowedStore = nil then
  begin
    WinCertCloseStore(WindowsCurrentUserDisallowedStore, 0);
    WindowsCurrentUserDisallowedStore := nil;
    ErrorDescription :=
      'Unable to open the local-machine Windows Disallowed certificate store.';
    Exit;
  end;
  Result := True;
end;

procedure CloseWindowsDisallowedStores;
begin
  if WindowsLocalMachineDisallowedStore <> nil then
    WinCertCloseStore(WindowsLocalMachineDisallowedStore, 0);
  if WindowsCurrentUserDisallowedStore <> nil then
    WinCertCloseStore(WindowsCurrentUserDisallowedStore, 0);
  WindowsLocalMachineDisallowedStore := nil;
  WindowsCurrentUserDisallowedStore := nil;
  WindowsVerifyError := '';
end;

function EncodedCertificateInStore(Store: Pointer; EncodedData: PByte;
  EncodedSize: DWORD; out LookupFailed: Boolean): Boolean;
var
  Certificate: PWinCertContext;
  Match: PWinCertContext;
  FindError: DWORD;
begin
  Result := False;
  LookupFailed := True;
  Certificate := WinCertCreateCertificateContext(X509_ASN_ENCODING,
    EncodedData, EncodedSize);
  if Certificate = nil then
    Exit;
  try
    Match := WinCertFindCertificateInStore(Store,
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, 0, CERT_FIND_EXISTING,
      Certificate, nil);
    if Match = nil then
    begin
      FindError := GetLastError;
      LookupFailed := FindError <> CRYPT_E_NOT_FOUND;
      Exit;
    end;
    WinCertFreeCertificateContext(Match);
    LookupFailed := False;
    Result := True;
  finally
    WinCertFreeCertificateContext(Certificate);
  end;
end;

function X509CertificateInStore(Store: Pointer; Certificate: PX509;
  out CheckFailed: Boolean): Boolean;
var
  Bio: PBIO;
  DER: AnsiString;
  DERSize: integer;
begin
  Result := False;
  CheckFailed := True;
  if Certificate = nil then
    Exit;
  Bio := BioNew(BioSMem);
  if Bio = nil then
    Exit;
  try
    if i2dX509bio(Bio, Certificate) <> 1 then
      Exit;
    DERSize := BioCtrlPending(Bio);
    if DERSize <= 0 then
      Exit;
    SetLength(DER, DERSize);
    if BioRead(Bio, DER, DERSize) <> DERSize then
      Exit;
    Result := EncodedCertificateInStore(Store, PByte(PAnsiChar(DER)),
      DWORD(DERSize), CheckFailed);
  finally
    BioFreeAll(Bio);
  end;
end;

function WindowsCertificateAllowsServerAuth(CertContext: PWinCertContext;
  out CheckFailed: Boolean): Boolean;
var
  UsageBuffer: Pointer;
  Usage: PWinCertEnhKeyUsage;
  UsageSize: DWORD;
  UsageError: DWORD;
  I: DWORD;
begin
  Result := False;
  CheckFailed := True;
  UsageBuffer := nil;
  UsageSize := 0;
  SetLastError(ERROR_SUCCESS);
  if not WinCertGetEnhancedKeyUsage(CertContext, 0, nil, UsageSize) then
  begin
    UsageError := GetLastError;
    if UsageError = CRYPT_E_NOT_FOUND then
    begin
      CheckFailed := False;
      Result := True;
    end;
    Exit;
  end;
  if (UsageSize < SizeOf(TWinCertEnhKeyUsage))
    or (UsageSize > DWORD(High(Integer))) then
    Exit;
  GetMem(UsageBuffer, UsageSize);
  try
    Usage := PWinCertEnhKeyUsage(UsageBuffer);
    SetLastError(ERROR_SUCCESS);
    if not WinCertGetEnhancedKeyUsage(CertContext, 0, Usage, UsageSize) then
      Exit;
    if Usage^.UsageIdentifierCount = 0 then
    begin
      UsageError := GetLastError;
      CheckFailed := False;
      Result := UsageError = CRYPT_E_NOT_FOUND;
      Exit;
    end;
    if Usage^.UsageIdentifiers = nil then
      Exit;
    for I := 0 to Usage^.UsageIdentifierCount - 1 do
      if (Usage^.UsageIdentifiers^[I] <> nil)
        and ((StrComp(Usage^.UsageIdentifiers^[I],
          ANY_ENHANCED_KEY_USAGE_OID) = 0)
        or (StrComp(Usage^.UsageIdentifiers^[I], SERVER_AUTH_OID) = 0)) then
      begin
        CheckFailed := False;
        Result := True;
        Exit;
      end;
    CheckFailed := False;
  finally
    FreeMem(UsageBuffer);
  end;
end;

function WindowsVerifyCallback(PreverifyOk: integer;
  StoreContext: PX509_STORE_CTX): integer; cdecl;
var
  Certificate: PX509;
  CheckFailed: Boolean;
begin
  Result := 0;
  if PreverifyOk = 0 then
    Exit;
  try
    Certificate := X509StoreCtxGetCurrentCert(StoreContext);
    if Certificate = nil then
    begin
      WindowsVerifyError :=
        'OpenSSL did not provide the current certificate in the peer chain.';
      Exit;
    end;
    if WindowsCurrentUserDisallowedStore = nil then
    begin
      WindowsVerifyError :=
        'Unable to open the current-user Windows Disallowed certificate store.';
      Exit;
    end;
    if WindowsLocalMachineDisallowedStore = nil then
    begin
      WindowsVerifyError :=
        'Unable to open the local-machine Windows Disallowed certificate store.';
      Exit;
    end;
    if X509CertificateInStore(WindowsCurrentUserDisallowedStore, Certificate,
      CheckFailed) then
    begin
      WindowsVerifyError :=
        'The Windows certificate policy distrusts a certificate in the peer chain.';
      Exit;
    end;
    if CheckFailed then
    begin
      WindowsVerifyError :=
        'Unable to check the current-user Windows Disallowed certificate store.';
      Exit;
    end;
    if X509CertificateInStore(WindowsLocalMachineDisallowedStore, Certificate,
      CheckFailed) then
    begin
      WindowsVerifyError :=
        'The Windows certificate policy distrusts a certificate in the peer chain.';
      Exit;
    end;
    if CheckFailed then
    begin
      WindowsVerifyError :=
        'Unable to check the local-machine Windows Disallowed certificate store.';
      Exit;
    end;
    Result := 1;
  except
    WindowsVerifyError :=
      'Unable to apply the Windows certificate distrust policy.';
    Result := 0;
  end;
end;

function ImportWindowsRootStore(Store: Pointer; CurrentUserDisallowed: Pointer;
  LocalMachineDisallowed: Pointer; OpenSSLStore: PX509_STORE;
  var LoadedCount: integer; out ErrorDescription: string): Boolean;
var
  CertContext: PWinCertContext;
  Bio: PBIO;
  Cert: PX509;
  DER: AnsiString;
  EnumError: DWORD;
  OpenSSLError: integer;
  UsageCheckFailed: Boolean;
  DisallowedLookupFailed: Boolean;
begin
  Result := False;
  CertContext := nil;
  try
    while True do
    begin
      CertContext := WinCertEnumCertificatesInStore(Store, CertContext);
      if CertContext = nil then
      begin
        EnumError := GetLastError;
        if (EnumError <> CRYPT_E_NOT_FOUND)
          and (EnumError <> ERROR_NO_MORE_FILES) then
        begin
          ErrorDescription :=
            'Unable to enumerate a Windows ROOT certificate store.';
          Exit;
        end;
        Break;
      end;
      if (CertContext^.EncodedSize = 0)
        or (CertContext^.EncodedSize > DWORD(High(Integer))) then
      begin
        ErrorDescription :=
          'A Windows ROOT certificate store contains an invalid certificate.';
        Exit;
      end;
      if not WindowsCertificateAllowsServerAuth(CertContext,
        UsageCheckFailed) then
      begin
        if UsageCheckFailed then
        begin
          ErrorDescription :=
            'Unable to check a Windows root certificate trust purpose.';
          Exit;
        end;
        Continue;
      end;
      if EncodedCertificateInStore(CurrentUserDisallowed,
        CertContext^.EncodedData, CertContext^.EncodedSize,
        DisallowedLookupFailed) then
        Continue;
      if DisallowedLookupFailed then
      begin
        ErrorDescription :=
          'Unable to check the current-user Windows Disallowed certificate store.';
        Exit;
      end;
      if EncodedCertificateInStore(LocalMachineDisallowed,
        CertContext^.EncodedData, CertContext^.EncodedSize,
        DisallowedLookupFailed) then
        Continue;
      if DisallowedLookupFailed then
      begin
        ErrorDescription :=
          'Unable to check the local-machine Windows Disallowed certificate store.';
        Exit;
      end;
      SetString(DER, PAnsiChar(CertContext^.EncodedData),
        CertContext^.EncodedSize);
      Bio := BioNew(BioSMem);
      if Bio = nil then
      begin
        ErrorDescription := 'OpenSSL could not allocate a certificate buffer.';
        Exit;
      end;
      try
        if BioWrite(Bio, DER, Length(DER)) <> Length(DER) then
        begin
          ErrorDescription := 'OpenSSL could not read a Windows root certificate.';
          Exit;
        end;
        Cert := d2iX509bio(Bio, nil);
        if Cert = nil then
        begin
          ErrorDescription := 'OpenSSL could not decode a Windows root certificate.';
          Exit;
        end;
        try
          ErrClearError;
          if X509StoreAddCert(OpenSSLStore, Cert) = 1 then
            Inc(LoadedCount)
          else
          begin
            OpenSSLError := ErrGetError;
            if (OpenSSLError and $fff) <> X509_R_CERT_ALREADY_IN_HASH_TABLE then
            begin
              ErrorDescription :=
                'OpenSSL could not trust a Windows root certificate.';
              Exit;
            end;
            ErrClearError;
            Inc(LoadedCount);
          end;
        finally
          X509Free(Cert);
        end;
      finally
        BioFreeAll(Bio);
      end;
    end;
    Result := True;
  finally
    if CertContext <> nil then
      WinCertFreeCertificateContext(CertContext);
  end;
end;

function LoadDefaultVerifyPaths(Ctx: PSSL_CTX;
  out ErrorDescription: string): Boolean;
var
  CurrentUserRoot: Pointer;
  LocalMachineRoot: Pointer;
  CurrentUserDisallowed: Pointer;
  LocalMachineDisallowed: Pointer;
  OpenSSLStore: PX509_STORE;
  LoadedCount: integer;
begin
  Result := False;
  ErrorDescription := '';
  LoadedCount := 0;
  if not SSLWindowsCertificateVerificationAvailable then
  begin
    ErrorDescription :=
      'Windows certificate policy checks require OpenSSL 1.0.2 or later.';
    Exit;
  end;
  CurrentUserRoot := OpenWindowsCertificateStore('ROOT',
    CERT_SYSTEM_STORE_CURRENT_USER);
  if CurrentUserRoot = nil then
  begin
    ErrorDescription :=
      'Unable to open the current-user Windows ROOT certificate store.';
    Exit;
  end;
  LocalMachineRoot := OpenWindowsCertificateStore('ROOT',
    CERT_SYSTEM_STORE_LOCAL_MACHINE);
  if LocalMachineRoot = nil then
  begin
    WinCertCloseStore(CurrentUserRoot, 0);
    ErrorDescription :=
      'Unable to open the local-machine Windows ROOT certificate store.';
    Exit;
  end;
  CurrentUserDisallowed := OpenWindowsCertificateStore('Disallowed',
    CERT_SYSTEM_STORE_CURRENT_USER);
  if CurrentUserDisallowed = nil then
  begin
    WinCertCloseStore(LocalMachineRoot, 0);
    WinCertCloseStore(CurrentUserRoot, 0);
    ErrorDescription :=
      'Unable to open the current-user Windows Disallowed certificate store.';
    Exit;
  end;
  LocalMachineDisallowed := OpenWindowsCertificateStore('Disallowed',
    CERT_SYSTEM_STORE_LOCAL_MACHINE);
  if LocalMachineDisallowed = nil then
  begin
    WinCertCloseStore(CurrentUserDisallowed, 0);
    WinCertCloseStore(LocalMachineRoot, 0);
    WinCertCloseStore(CurrentUserRoot, 0);
    ErrorDescription :=
      'Unable to open the local-machine Windows Disallowed certificate store.';
    Exit;
  end;
  try
    OpenSSLStore := SslCtxGetCertStore(Ctx);
    if OpenSSLStore = nil then
    begin
      ErrorDescription := 'OpenSSL did not provide a certificate store.';
      Exit;
    end;
    if not ImportWindowsRootStore(CurrentUserRoot, CurrentUserDisallowed,
      LocalMachineDisallowed, OpenSSLStore, LoadedCount,
      ErrorDescription) then
      Exit;
    if not ImportWindowsRootStore(LocalMachineRoot, CurrentUserDisallowed,
      LocalMachineDisallowed, OpenSSLStore, LoadedCount,
      ErrorDescription) then
      Exit;
    if LoadedCount = 0 then
    begin
      ErrorDescription := 'The Windows certificate stores contain no trusted roots.';
      Exit;
    end;
    Result := True;
  finally
    WinCertCloseStore(LocalMachineDisallowed, 0);
    WinCertCloseStore(CurrentUserDisallowed, 0);
    WinCertCloseStore(LocalMachineRoot, 0);
    WinCertCloseStore(CurrentUserRoot, 0);
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF MSWINDOWS}
function LoadDefaultVerifyPaths(Ctx: PSSL_CTX;
  out ErrorDescription: string): Boolean;
begin
  Result := SslCtxSetDefaultVerifyPaths(Ctx) = 1;
  if Result then
    ErrorDescription := ''
  else
    ErrorDescription := 'OpenSSL could not load the default certificate paths.';
end;
{$ELSE}
{$IFDEF CIL}
function LoadDefaultVerifyPaths(Ctx: PSSL_CTX;
  out ErrorDescription: string): Boolean;
begin
  Result := SslCtxSetDefaultVerifyPaths(Ctx) = 1;
  if Result then
    ErrorDescription := ''
  else
    ErrorDescription := 'OpenSSL could not load the default certificate paths.';
end;
{$ENDIF}
{$ENDIF}

{==============================================================================}

{$IFNDEF CIL}
function PasswordCallback(buf:PAnsiChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: AnsiString;
begin
  Password := '';
  if TCustomSSL(userdata) is TCustomSSL then
    Password := TCustomSSL(userdata).KeyPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  StrLCopy(buf, PAnsiChar(Password + #0), Result + 1);
end;
{$ENDIF}

{==============================================================================}

constructor TSSLOpenSSL.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FCiphers := 'DEFAULT';
  FSsl := nil;
  Fctx := nil;
end;

destructor TSSLOpenSSL.Destroy;
begin
  DeInit;
  inherited Destroy;
end;

function TSSLOpenSSL.LibVersion: String;
begin
  Result := SSLeayversion(0);
end;

function TSSLOpenSSL.LibName: String;
begin
  Result := 'ssl_openssl';
end;

function TSSLOpenSSL.SSLCheck: Boolean;
var
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
  s : AnsiString;
begin
  Result := true;
  FLastErrorDesc := '';
  FLastError := ErrGetError;
  ErrClearError;
  if FLastError <> 0 then
  begin
    Result := False;
{$IFDEF CIL}
    sb := StringBuilder.Create(256);
    ErrErrorString(FLastError, sb, 256);
    FLastErrorDesc := Trim(sb.ToString);
{$ELSE}
    s := StringOfChar(#0, 256);
    ErrErrorString(FLastError, s, Length(s));
    FLastErrorDesc := s;
{$ENDIF}
  end;
end;

function TSSLOpenSSL.CreateSelfSignedCert(Host: string): Boolean;
var
  pk: EVP_PKEY;
  x: PX509;
  rsa: PRSA;
  t: PASN1_UTCTIME;
  name: PX509_NAME;
  b: PBIO;
  xn, y: integer;
  s: AnsiString;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  Result := True;
  pk := EvpPkeynew;
  x := X509New;
  try
    rsa := RsaGenerateKey(1024, $10001, nil, nil);
    EvpPkeyAssign(pk, EVP_PKEY_RSA, rsa);
    X509SetVersion(x, 2);
    Asn1IntegerSet(X509getSerialNumber(x), 0);
    t := Asn1UtctimeNew;
    try
      X509GmtimeAdj(t, -60 * 60 *24);
      X509SetNotBefore(x, t);
      X509GmtimeAdj(t, 60 * 60 * 60 *24);
      X509SetNotAfter(x, t);
    finally
      Asn1UtctimeFree(t);
    end;
    X509SetPubkey(x, pk);
    Name := X509GetSubjectName(x);
    X509NameAddEntryByTxt(Name, 'C', $1001, 'CZ', -1, -1, 0);
    X509NameAddEntryByTxt(Name, 'CN', $1001, host, -1, -1, 0);
    x509SetIssuerName(x, Name);
    x509Sign(x, pk, EvpGetDigestByName('SHA1'));
    b := BioNew(BioSMem);
    try
      i2dX509Bio(b, x);
      xn := bioctrlpending(b);
{$IFDEF CIL}
      sb := StringBuilder.Create(xn);
      y := bioread(b, sb, xn);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
{$ELSE}
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
{$ENDIF}
    finally
      BioFreeAll(b);
    end;
    FCertificate := s;
    b := BioNew(BioSMem);
    try
      i2dPrivatekeyBio(b, pk);
      xn := bioctrlpending(b);
{$IFDEF CIL}
      sb := StringBuilder.Create(xn);
      y := bioread(b, sb, xn);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
{$ELSE}
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
{$ENDIF}
    finally
      BioFreeAll(b);
    end;
    FPrivatekey := s;
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

function TSSLOpenSSL.LoadPFX(pfxdata: Ansistring): Boolean;
var
  cert, pkey, ca: SslPtr;
  b: PBIO;
  p12: SslPtr;
begin
  Result := False;
  b := BioNew(BioSMem);
  try
    BioWrite(b, pfxdata, Length(PfxData));
    p12 := d2iPKCS12bio(b, nil);
    if not Assigned(p12) then
      Exit;
    try
      cert := nil;
      pkey := nil;
      ca := nil;
      try {pf}
        if PKCS12parse(p12, FKeyPassword, pkey, cert, ca) > 0 then
          if SSLCTXusecertificate(Fctx, cert) > 0 then
            if SSLCTXusePrivateKey(Fctx, pkey) > 0 then
              Result := True;
      {pf}
      finally
        EvpPkeyFree(pkey);
        X509free(cert);
        SkX509PopFree(ca,_X509Free); // for ca=nil a new STACK was allocated...
      end;
      {/pf}
    finally
      PKCS12free(p12);
    end;
  finally
    BioFreeAll(b);
  end;
end;

function TSSLOpenSSL.SetSslKeys(server:Boolean): boolean;
var
  st: TFileStream;
  s, DefaultVerifyError: string;
  DefaultVerifyFailed: Boolean;
begin
  Result := False;
  DefaultVerifyFailed := False;
  if not assigned(FCtx) then
    Exit;
  try
    if FCertificateFile <> '' then
      if SslCtxUseCertificateChainFile(FCtx, FCertificateFile) <> 1 then
        if SslCtxUseCertificateFile(FCtx, FCertificateFile, SSL_FILETYPE_PEM) <> 1 then
          if SslCtxUseCertificateFile(FCtx, FCertificateFile, SSL_FILETYPE_ASN1) <> 1 then
            Exit;
    if FCertificate <> '' then
      if SslCtxUseCertificateASN1(FCtx, length(FCertificate), FCertificate) <> 1 then
        Exit;
    SSLCheck;
    if FPrivateKeyFile <> '' then
      if SslCtxUsePrivateKeyFile(FCtx, FPrivateKeyFile, SSL_FILETYPE_PEM) <> 1 then
        if SslCtxUsePrivateKeyFile(FCtx, FPrivateKeyFile, SSL_FILETYPE_ASN1) <> 1 then
          Exit;
    if FPrivateKey <> '' then
      if SslCtxUsePrivateKeyASN1(EVP_PKEY_RSA, FCtx, FPrivateKey, length(FPrivateKey)) <> 1 then
        Exit;
    SSLCheck;
    if FCertCAFile <> '' then
      if SslCtxLoadVerifyLocations(FCtx, FCertCAFile, '') <> 1 then
        Exit;
    if not server and FVerifyCert and (FCertCAFile = '') then
      if not LoadDefaultVerifyPaths(FCtx, DefaultVerifyError) then
      begin
        DefaultVerifyFailed := True;
        Exit;
      end;
    if FPFXfile <> '' then
    begin
      try
        st := TFileStream.Create(FPFXfile, fmOpenRead	 or fmShareDenyNone);
        try
          s := ReadStrFromStream(st, st.Size);
        finally
          st.Free;
        end;
        if not LoadPFX(s) then
          Exit;
      except
        on Exception do
          Exit;
      end;
    end;
    if FPFX <> '' then
      if not LoadPFX(FPfx) then
        Exit;
    SSLCheck;
    Result := True;
  finally
    SSLCheck;
    if DefaultVerifyFailed then
    begin
      FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
      FLastErrorDesc := DefaultVerifyError;
    end;
  end;
end;

function TSSLOpenSSL.Init(server:Boolean): Boolean;
var
  s: AnsiString;
begin
  Result := False;
  FLastErrorDesc := '';
  FLastError := 0;
  Fctx := nil;
  case FSSLType of
    LT_TLSv1:
      Fctx := SslCtxNew(SslMethodTLSV1);
    LT_TLSv1_1:
      Fctx := SslCtxNew(SslMethodTLSV11);
    LT_TLSv1_2:
      Fctx := SslCtxNew(SslMethodTLSV12);
    LT_all:
      begin
        //try new call for OpenSSL 1.1.0 first
        Fctx := SslCtxNew(SslMethodTLS);
        if Fctx=nil then
          //callback to previous versions
          Fctx := SslCtxNew(SslMethodV23);
      end;
  else
    Exit;
  end;
  if Fctx = nil then
  begin
    SSLCheck;
    Exit;
  end
  else
  begin
    s := FCiphers;
    SslCtxSetCipherList(Fctx, s);
    if FVerifyCert then
    begin
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
      if not SSLWindowsCertificateVerificationAvailable then
      begin
        FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
        FLastErrorDesc :=
          'Windows certificate policy checks require OpenSSL 1.0.2 or later.';
        Exit;
      end;
      SslCtxSetVerify(FCtx, SSL_VERIFY_PEER,
        @WindowsVerifyCallback)
{$ELSE}
      SslCtxSetVerify(FCtx, SSL_VERIFY_PEER, nil)
{$ENDIF}
{$ELSE}
      SslCtxSetVerify(FCtx, SSL_VERIFY_PEER, nil)
{$ENDIF}
    end
    else
      SslCtxSetVerify(FCtx, SSL_VERIFY_NONE, nil);
{$IFNDEF CIL}
    SslCtxSetDefaultPasswdCb(FCtx, @PasswordCallback);
    SslCtxSetDefaultPasswdCbUserdata(FCtx, self);
{$ENDIF}

    if server and (FCertificateFile = '') and (FCertificate = '')
      and (FPFXfile = '') and (FPFX = '') then
    begin
      CreateSelfSignedcert(FSocket.ResolveIPToName(FSocket.GetRemoteSinIP));
    end;

    if not SetSSLKeys(server) then
      Exit
    else
    begin
      Fssl := nil;
      Fssl := SslNew(Fctx);
      if Fssl = nil then
      begin
        SSLCheck;
        exit;
      end;
    end;
  end;
  Result := true;
end;

function TSSLOpenSSL.DeInit: Boolean;
begin
  Result := True;
  if assigned (Fssl) then
    sslfree(Fssl);
  Fssl := nil;
  if assigned (Fctx) then
  begin
    SslCtxFree(Fctx);
    Fctx := nil;
    ErrRemoveState(0);
  end;
  FSSLEnabled := False;
end;

function TSSLOpenSSL.Prepare(server:Boolean): Boolean;
begin
  Result := false;
  DeInit;
  if Init(server) then
    Result := true
  else
    DeInit;
end;

procedure TSSLOpenSSL.SetWaitError;
begin
  if FSocket.LastError <> 0 then
  begin
    FLastError:=FSocket.LastError;
    FLastErrorDesc:=FSocket.LastErrorDesc;
  end
  else
  begin
    FLastError:=WSAETIMEDOUT;
    FLastErrorDesc:=TBlockSocket.GetErrorDesc(WSAETIMEDOUT);
  end;
end;

function TSSLOpenSSL.WaitForIO(Error: integer; StartTick: LongWord): Boolean;
var
  Timeout: integer;
begin
  Timeout:=DataTimeout - integer(TickDelta(StartTick, GetTick));
  if Timeout >= 0 then
    if Error = SSL_ERROR_WANT_READ then
      Result:=FSocket.CanRead(Timeout)
    else
      Result:=FSocket.CanWrite(Timeout)
  else
    Result:=False;
  if not Result then
    SetWaitError;
end;

function NormalizeTLSIdentityHost(const Host: AnsiString;
  out IsIPAddress: Boolean): AnsiString;
var
  ZoneDelimiter: integer;
begin
  Result := Host;
  ZoneDelimiter := Pos('%', Result);
  if ZoneDelimiter <> 0 then
  begin
    Result := Copy(Result, 1, ZoneDelimiter - 1);
    if Pos(':', Result) <> 0 then
    begin
      IsIPAddress := True;
      Exit;
    end;
    Result := Host;
  end;
  IsIPAddress := IsIP(Result) or IsIP6(Result) or (Pos(':', Result) <> 0);
end;

function TSSLOpenSSL.Connect: boolean;
var
  x: integer;
  b: boolean;
  err: integer;
  IdentityHost: AnsiString;
  IdentityIsIPAddress: Boolean;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  WindowsStoreError: string;
{$ENDIF}
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  CloseWindowsDisallowedStores;
{$ENDIF}
{$ENDIF}
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  if FVerifyCert then
    if not OpenWindowsDisallowedStores(WindowsStoreError) then
    begin
      FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
      FLastErrorDesc := WindowsStoreError;
      Exit;
    end;
  try
{$ENDIF}
{$ENDIF}
  if Prepare(False) then
  begin
{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Socket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket.Socket) < 1 then
{$ENDIF}
    begin
      SSLCheck;
      Exit;
    end;
    if FVerifyCert and (SNIHost <> '') then
      if not ConfigureHostVerification then
        Exit;
    IdentityHost := NormalizeTLSIdentityHost(AnsiString(SNIHost),
      IdentityIsIPAddress);
    if (IdentityHost <> '') and not IdentityIsIPAddress then
      SSLCtrl(Fssl, SSL_CTRL_SET_TLSEXT_HOSTNAME,
        TLSEXT_NAMETYPE_host_name, PAnsiChar(IdentityHost));
    if FSocket.ConnectionTimeout <= 0 then //do blocking call of SSL_Connect
    begin
      x := sslconnect(FSsl);
      if x < 1 then
      begin
        SSLcheck;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
        if WindowsVerifyError <> '' then
        begin
          FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
          FLastErrorDesc := WindowsVerifyError;
        end;
{$ENDIF}
{$ENDIF}
        Exit;
      end;
    end
    else //do non-blocking call of SSL_Connect
    begin
      b := Fsocket.NonBlockMode;
      Fsocket.NonBlockMode := true;
      try
        repeat
          if FSocket.StopFlag then
          begin
            FSocket.StopFlag:=False;
            FLastError:=WSAECONNABORTED;
            FLastErrorDesc:=TBlockSocket.GetErrorDesc(WSAECONNABORTED);
            Exit;
          end;
          x := sslconnect(FSsl);
          err := SslGetError(FSsl, x);
          if err = SSL_ERROR_WANT_READ then
            if not FSocket.CanRead(FSocket.ConnectionTimeout) then
            begin
              SetWaitError;
              Exit;
            end;
          if err = SSL_ERROR_WANT_WRITE then
            if not FSocket.CanWrite(FSocket.ConnectionTimeout) then
            begin
              SetWaitError;
              Exit;
            end;
        until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
      finally
        Fsocket.NonBlockMode := b;
      end;
      if err <> SSL_ERROR_NONE then
      begin
        SSLcheck;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
        if WindowsVerifyError <> '' then
        begin
          FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
          FLastErrorDesc := WindowsVerifyError;
        end;
{$ENDIF}
{$ENDIF}
        Exit;
      end;
    end;
  if FverifyCert then
    if (GetVerifyCert <> 0) or (not DoVerifyCert) then
      Exit;
    FSSLEnabled := True;
    Result := True;
  end;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  finally
    CloseWindowsDisallowedStores;
  end;
{$ENDIF}
{$ENDIF}
end;

function TSSLOpenSSL.ConfigureHostVerification: Boolean;
var
  Param: PX509_VERIFY_PARAM;
  Host: AnsiString;
  HostIsIPAddress: Boolean;
begin
  Result := False;
  Host := NormalizeTLSIdentityHost(AnsiString(SNIHost), HostIsIPAddress);
  if Host = '' then
  begin
    FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
    FLastErrorDesc := 'TLS hostname verification requires a target hostname.';
    Exit;
  end;
  if not SSLHostVerificationAvailable then
  begin
    FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
    FLastErrorDesc := 'TLS hostname verification requires OpenSSL 1.0.2 or later.';
    Exit;
  end;
{$IFDEF CIL}
  try
{$ENDIF}
  Param := SslGet0Param(Fssl);
  if Param = nil then
  begin
    FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
    FLastErrorDesc := 'OpenSSL did not provide certificate verification parameters.';
    Exit;
  end;
  if HostIsIPAddress then
    Result := X509VerifyParamSet1IPAsc(Param, Host) = 1
  else
  begin
    X509VerifyParamSetHostFlags(Param, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    Result := X509VerifyParamSet1Host(Param, Host, 0) = 1;
  end;
  if not Result then
  begin
    if SSLCheck then
    begin
      FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
      FLastErrorDesc := 'OpenSSL rejected the TLS verification hostname.';
    end;
  end;
{$IFDEF CIL}
  except
    on Exception do
    begin
      Result := False;
      FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
      FLastErrorDesc :=
        'TLS hostname verification is unavailable in the loaded OpenSSL runtime.';
    end;
  end;
{$ENDIF}
end;

function TSSLOpenSSL.Accept: boolean;
var
  x: integer;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  WindowsStoreError: string;
{$ENDIF}
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  CloseWindowsDisallowedStores;
{$ENDIF}
{$ENDIF}
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  if FVerifyCert then
    if not OpenWindowsDisallowedStores(WindowsStoreError) then
    begin
      FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
      FLastErrorDesc := WindowsStoreError;
      Exit;
    end;
  try
{$ENDIF}
{$ENDIF}
  if Prepare(True) then
  begin
{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Socket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket.Socket) < 1 then
{$ENDIF}
    begin
      SSLCheck;
      Exit;
    end;
    x := sslAccept(FSsl);
    if x < 1 then
    begin
      SSLcheck;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
      if WindowsVerifyError <> '' then
      begin
        FLastError := X509_V_ERR_APPLICATION_VERIFICATION;
        FLastErrorDesc := WindowsVerifyError;
      end;
{$ENDIF}
{$ENDIF}
      Exit;
    end;
    FSSLEnabled := True;
    Result := True;
  end;
{$IFDEF MSWINDOWS}
{$IFNDEF CIL}
  finally
    CloseWindowsDisallowedStores;
  end;
{$ENDIF}
{$ENDIF}
end;

function TSSLOpenSSL.Shutdown: boolean;
begin
  if assigned(FSsl) then
    sslshutdown(FSsl);
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.BiShutdown: boolean;
var
  x: integer;
begin
  if assigned(FSsl) then
  begin
    x := sslshutdown(FSsl);
    if x = 0 then
    begin
      Synsock.Shutdown(FSocket.Socket, 1);
      sslshutdown(FSsl);
    end;
  end;
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
  StartTick: LongWord;
  OldNonBlockMode: Boolean;
{$IFDEF CIL}
  s: ansistring;
{$ENDIF}
begin
  FLastError := 0;
  FLastErrorDesc := '';
  StartTick:=GetTick;
  OldNonBlockMode:=FSocket.NonBlockMode;
  if (DataTimeout >= 0) and not OldNonBlockMode then
    FSocket.NonBlockMode:=True;
  try
    repeat
      if FSocket.StopFlag then
      begin
        FSocket.StopFlag:=False;
        FLastError:=WSAECONNABORTED;
        FLastErrorDesc:=TBlockSocket.GetErrorDesc(WSAECONNABORTED);
        Result:=0;
        Exit;
      end;
{$IFDEF CIL}
      s := StringOf(Buffer);
      Result := SslWrite(FSsl, s, Len);
{$ELSE}
      Result := SslWrite(FSsl, Buffer , Len);
{$ENDIF}
      err := SslGetError(FSsl, Result);
      if (DataTimeout >= 0) and
         ((err = SSL_ERROR_WANT_READ) or (err = SSL_ERROR_WANT_WRITE)) then
        if not WaitForIO(err, StartTick) then
        begin
          Result:=0;
          Exit;
        end;
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  finally
    if (DataTimeout >= 0) and not OldNonBlockMode then
      FSocket.NonBlockMode:=OldNonBlockMode;
  end;
  if err = SSL_ERROR_ZERO_RETURN then
  begin
    Result:=0;
    FLastError:=WSAECONNRESET;
    FLastErrorDesc:=TBlockSocket.GetErrorDesc(WSAECONNRESET);
  end
  else
    if (err <> 0) then
      FLastError:=err;
end;

function TSSLOpenSSL.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
  StartTick: LongWord;
  OldNonBlockMode: Boolean;
{$IFDEF CIL}
  sb: stringbuilder;
  s: ansistring;
{$ENDIF}
begin
  FLastError := 0;
  FLastErrorDesc := '';
  StartTick:=GetTick;
  OldNonBlockMode:=FSocket.NonBlockMode;
  if (DataTimeout >= 0) and not OldNonBlockMode then
    FSocket.NonBlockMode:=True;
  try
    repeat
      if FSocket.StopFlag then
      begin
        FSocket.StopFlag:=False;
        FLastError:=WSAECONNABORTED;
        FLastErrorDesc:=TBlockSocket.GetErrorDesc(WSAECONNABORTED);
        Result:=0;
        Exit;
      end;
{$IFDEF CIL}
      sb := StringBuilder.Create(Len);
      Result := SslRead(FSsl, sb, Len);
      if Result > 0 then
      begin
        sb.Length := Result;
        s := sb.ToString;
        System.Array.Copy(BytesOf(s), Buffer, length(s));
      end;
{$ELSE}
      Result := SslRead(FSsl, Buffer , Len);
{$ENDIF}
      err := SslGetError(FSsl, Result);
      if (DataTimeout >= 0) and
         ((err = SSL_ERROR_WANT_READ) or (err = SSL_ERROR_WANT_WRITE)) then
        if not WaitForIO(err, StartTick) then
        begin
          Result:=0;
          Exit;
        end;
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  finally
    if (DataTimeout >= 0) and not OldNonBlockMode then
      FSocket.NonBlockMode:=OldNonBlockMode;
  end;
  if err = SSL_ERROR_ZERO_RETURN then
    Result:=0
  {pf}// Verze 1.1.0 byla s else tak jak to ted mam,
      // ve verzi 1.1.1 bylo ELSE zruseno, ale pak je SSL_ERROR_ZERO_RETURN
      // propagovano jako Chyba.
  {pf} else {/pf} if (err <> 0) then
    FLastError := err;
end;

function TSSLOpenSSL.WaitingData: Integer;
begin
  Result := sslpending(Fssl);
end;

function TSSLOpenSSL.GetSSLVersion: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSlGetVersion(FSsl);
end;

function TSSLOpenSSL.GetPeerSubject: string;
var
  cert: PX509;
  s: ansistring;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := X509NameOneline(X509GetSubjectName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := X509NameOneline(X509GetSubjectName(cert), s, Length(s));
{$ENDIF}
  X509Free(cert);
end;


function TSSLOpenSSL.GetPeerSerialNo: integer; {pf}
var
  cert: PX509;
  SN:   PASN1_INTEGER;
begin
  if not assigned(FSsl) then
  begin
    Result := -1;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := -1;
      Exit;
    end;
    SN := X509GetSerialNumber(cert);
    Result := Asn1IntegerGet(SN);
  finally
    X509Free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerName: string;
var
  s: ansistring;
begin
  s := GetPeerSubject;
  s := SeparateRight(s, '/CN=');
  Result := Trim(SeparateLeft(s, '/'));
end;

function TSSLOpenSSL.GetPeerNameHash: cardinal; {pf}
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := 0;
      Exit;
    end;
    Result := X509NameHash(X509GetSubjectName(cert));
  finally
    X509Free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerIssuer: string;
var
  cert: PX509;
  s: ansistring;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := X509NameOneline(X509GetIssuerName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := X509NameOneline(X509GetIssuerName(cert), s, Length(s));
{$ENDIF}
  X509Free(cert);
end;

function TSSLOpenSSL.GetPeerFingerprint: string;
var
  cert: PX509;
  x: integer;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(EVP_MAX_MD_SIZE);
  X509Digest(cert, EvpGetDigestByName('MD5'), sb, x);
  sb.Length := x;
  Result := sb.ToString;
{$ELSE}
  setlength(Result, EVP_MAX_MD_SIZE);
  X509Digest(cert, EvpGetDigestByName('MD5'), Result, x);
  SetLength(Result, x);
{$ENDIF}
  X509Free(cert);
end;

function TSSLOpenSSL.GetCertInfo: string;
var
  cert: PX509;
  x, y: integer;
  b: PBIO;
  s: AnsiString;
{$IFDEF CIL}
  sb: stringbuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  try {pf}
    b := BioNew(BioSMem);
    try
      X509Print(b, cert);
      x := bioctrlpending(b);
  {$IFDEF CIL}
      sb := StringBuilder.Create(x);
      y := bioread(b, sb, x);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
  {$ELSE}
      setlength(s,x);
      y := bioread(b,s,x);
      if y > 0 then
        setlength(s, y);
  {$ENDIF}
      Result := ReplaceString(s, LF, CRLF);
    finally
      BioFreeAll(b);
    end;
  {pf}
  finally
    X509Free(cert);
  end;
  {/pf}
end;

function TSSLOpenSSL.GetCipherName: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SslCipherGetName(SslGetCurrentCipher(FSsl));
end;

function TSSLOpenSSL.GetCipherBits: integer;
var
  x: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    Result := SSLCipherGetBits(SslGetCurrentCipher(FSsl), x);
end;

function TSSLOpenSSL.GetCipherAlgBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    SSLCipherGetBits(SslGetCurrentCipher(FSsl), Result);
end;

function TSSLOpenSSL.GetVerifyCert: integer;
begin
  if not assigned(FSsl) then
    Result := 1
  else
    Result := SslGetVerifyResult(FSsl);
end;

{==============================================================================}

initialization
  if InitSSLInterface then
    SSLImplementation := TSSLOpenSSL;

end.
