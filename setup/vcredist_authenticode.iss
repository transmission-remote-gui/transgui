const
  AUTH_CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
  AUTH_CSTR_EQUAL = 2;
  AUTH_GPTR = $0040;
  AUTH_WTD_CHOICE_FILE = 1;
  AUTH_WTD_STATEACTION_VERIFY = 1;
  AUTH_WTD_STATEACTION_CLOSE = 2;
  AUTH_WTD_UI_NONE = 2;

type
  TAuthWinTrustFileInfo = record
    StructSize: DWORD;
    FilePath: DWORD;
    FileHandle: DWORD;
    KnownSubject: DWORD;
  end;

  TAuthWinTrustData = record
    StructSize: DWORD;
    PolicyCallbackData: DWORD;
    SIPClientData: DWORD;
    UIChoice: DWORD;
    RevocationChecks: DWORD;
    UnionChoice: DWORD;
    FileInfo: DWORD;
    StateAction: DWORD;
    StateData: DWORD;
    URLReference: DWORD;
    ProviderFlags: DWORD;
    UIContext: DWORD;
  end;

  TAuthCryptProviderCertPrefix = record
    StructSize: DWORD;
    CertContext: DWORD;
  end;

function AuthGlobalAlloc(Flags, Bytes: DWORD): DWORD;
  external 'GlobalAlloc@kernel32.dll stdcall delayload';
function AuthGlobalFree(Memory: DWORD): DWORD;
  external 'GlobalFree@kernel32.dll stdcall delayload';
function AuthCopyWideString(Destination: DWORD; const Source: String;
  MaxLength: Integer): DWORD;
  external 'lstrcpynW@kernel32.dll stdcall delayload';
procedure AuthCopyFileInfo(Destination: DWORD;
  var Source: TAuthWinTrustFileInfo; Bytes: DWORD);
  external 'RtlMoveMemory@kernel32.dll stdcall delayload';
procedure AuthCopyProviderCert(
  var Destination: TAuthCryptProviderCertPrefix; Source, Bytes: DWORD);
  external 'RtlMoveMemory@kernel32.dll stdcall delayload';
function AuthWinVerifyTrust(WindowHandle: Integer; var ActionID: TGUID;
  var TrustData: TAuthWinTrustData): Integer;
  external 'WinVerifyTrust@wintrust.dll stdcall delayload';
function AuthGetProviderData(StateData: DWORD): DWORD;
  external 'WTHelperProvDataFromStateData@wintrust.dll stdcall delayload';
function AuthGetProviderSigner(ProviderData, SignerIndex: DWORD;
  CounterSigner: BOOL; CounterSignerIndex: DWORD): DWORD;
  external 'WTHelperGetProvSignerFromChain@wintrust.dll stdcall delayload';
function AuthGetProviderCert(Signer: DWORD; CertIndex: DWORD): DWORD;
  external 'WTHelperGetProvCertFromChain@wintrust.dll stdcall delayload';
function AuthGetCertName(CertContext, NameType, Flags, TypeParameter,
  NameBuffer, NameBufferLength: DWORD): DWORD;
  external 'CertGetNameStringW@crypt32.dll stdcall delayload';
function AuthCompareStringOrdinal(String1: DWORD; String1Length: Integer;
  const String2: String; String2Length: Integer;
  IgnoreCase: BOOL): Integer;
  external 'CompareStringOrdinal@kernel32.dll stdcall delayload';

procedure AuthFreeMemory(var Memory: DWORD);
begin
  if Memory = 0 then
    Exit;

  try
    if AuthGlobalFree(Memory) <> 0 then
      Log('Could not release an Authenticode verification buffer.');
  except
    Log('Could not release an Authenticode verification buffer: ' +
      GetExceptionMessage);
  end;
  Memory := 0;
end;

function VerifyMicrosoftAuthenticodeSignature(const FileName: String;
  var TrustStatus: Integer; var FailureReason: String): Boolean;
var
  ActionID: TGUID;
  CertNameBuffer: DWORD;
  CertNameLength: DWORD;
  CertPrefix: TAuthCryptProviderCertPrefix;
  FileInfo: TAuthWinTrustFileInfo;
  FileInfoBuffer: DWORD;
  PathBuffer: DWORD;
  ProviderCert: DWORD;
  ProviderData: DWORD;
  ProviderSigner: DWORD;
  TrustData: TAuthWinTrustData;
  VerificationAttempted: Boolean;
begin
  Result := False;
  TrustStatus := -1;
  FailureReason := '';
  CertNameBuffer := 0;
  FileInfoBuffer := 0;
  PathBuffer := 0;
  VerificationAttempted := False;

  try
    try
      PathBuffer := AuthGlobalAlloc(AUTH_GPTR, (Length(FileName) + 1) * 2);
      if PathBuffer = 0 then
      begin
        FailureReason := 'Could not allocate the verification path buffer.';
        Exit;
      end;
      if AuthCopyWideString(PathBuffer, FileName,
        Length(FileName) + 1) = 0 then
      begin
        FailureReason := 'Could not copy the verification path.';
        Exit;
      end;

      { Inno Setup 5.6.1 runs 32-bit Pascal Script code, so all native
        pointers in these WinTrust structures are four bytes. }
      FileInfo.StructSize := 16;
      FileInfo.FilePath := PathBuffer;
      FileInfo.FileHandle := 0;
      FileInfo.KnownSubject := 0;
      FileInfoBuffer := AuthGlobalAlloc(AUTH_GPTR, FileInfo.StructSize);
      if FileInfoBuffer = 0 then
      begin
        FailureReason := 'Could not allocate the WinTrust file information.';
        Exit;
      end;
      AuthCopyFileInfo(FileInfoBuffer, FileInfo, FileInfo.StructSize);

      TrustData.StructSize := 48;
      TrustData.PolicyCallbackData := 0;
      TrustData.SIPClientData := 0;
      TrustData.UIChoice := AUTH_WTD_UI_NONE;
      TrustData.RevocationChecks := 0;
      TrustData.UnionChoice := AUTH_WTD_CHOICE_FILE;
      TrustData.FileInfo := FileInfoBuffer;
      TrustData.StateAction := AUTH_WTD_STATEACTION_VERIFY;
      TrustData.StateData := 0;
      TrustData.URLReference := 0;
      TrustData.ProviderFlags := 0;
      TrustData.UIContext := 0;

      ActionID := StringToGUID('{00AAC56B-CD44-11D0-8CC2-00C04FC295EE}');
      VerificationAttempted := True;
      TrustStatus := AuthWinVerifyTrust(0, ActionID, TrustData);
      if TrustStatus <> 0 then
      begin
        FailureReason := 'WinTrust rejected the file.';
        Exit;
      end;

      ProviderData := AuthGetProviderData(TrustData.StateData);
      if ProviderData = 0 then
      begin
        FailureReason := 'WinTrust did not return provider data.';
        Exit;
      end;
      ProviderSigner := AuthGetProviderSigner(ProviderData, 0, False, 0);
      if ProviderSigner = 0 then
      begin
        FailureReason := 'WinTrust did not return a primary signer.';
        Exit;
      end;
      ProviderCert := AuthGetProviderCert(ProviderSigner, 0);
      if ProviderCert = 0 then
      begin
        FailureReason := 'WinTrust did not return a signer certificate.';
        Exit;
      end;

      CertPrefix.StructSize := 0;
      CertPrefix.CertContext := 0;
      AuthCopyProviderCert(CertPrefix, ProviderCert, 8);
      if CertPrefix.CertContext = 0 then
      begin
        FailureReason := 'The signer certificate context is missing.';
        Exit;
      end;

      CertNameLength := AuthGetCertName(CertPrefix.CertContext,
        AUTH_CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, 0, 0, 0);
      if CertNameLength <= 1 then
      begin
        FailureReason := 'Could not read the signer certificate name.';
        Exit;
      end;
      CertNameBuffer := AuthGlobalAlloc(AUTH_GPTR, CertNameLength * 2);
      if CertNameBuffer = 0 then
      begin
        FailureReason := 'Could not allocate the signer name buffer.';
        Exit;
      end;
      if AuthGetCertName(CertPrefix.CertContext,
        AUTH_CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, 0, CertNameBuffer,
        CertNameLength) <= 1 then
      begin
        FailureReason := 'Could not copy the signer certificate name.';
        Exit;
      end;
      if AuthCompareStringOrdinal(CertNameBuffer, -1,
        'Microsoft Corporation', -1, False) <> AUTH_CSTR_EQUAL then
      begin
        FailureReason := 'The signer is not Microsoft Corporation.';
        Exit;
      end;

      Result := True;
    except
      FailureReason := 'Authenticode verification raised an exception: ' +
        GetExceptionMessage;
      Result := False;
    end;
  finally
    if VerificationAttempted then
    begin
      TrustData.StateAction := AUTH_WTD_STATEACTION_CLOSE;
      try
        AuthWinVerifyTrust(0, ActionID, TrustData);
      except
        Log('Could not release WinTrust state: ' + GetExceptionMessage);
      end;
    end;
    AuthFreeMemory(CertNameBuffer);
    AuthFreeMemory(FileInfoBuffer);
    AuthFreeMemory(PathBuffer);
  end;
end;
