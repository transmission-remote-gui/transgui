program openssl_loader_probe;

{$mode objfpc}{$H+}

uses
  SysUtils, ssl_openssl_lib;

procedure Fail(const MessageText: string);
begin
  WriteLn(StdErr, MessageText);
  Halt(1);
end;

var
  Context: PSSL_CTX;
  ConfiguredUtilName, ConfiguredSSLName: string;
{$IFDEF DARWIN}
  ExpectedFirstUtilFile, ExpectedFirstSSLFile: string;
{$ENDIF}
  ExpectedLibDir, ExpectedUtilFile, ExpectedSSLFile, VersionText: string;
  ExpectFailure, Loaded: Boolean;
  Method: PSSL_METHOD;
begin
  if (ParamCount < 1) or (ParamCount > 2) then
    Fail('Usage: openssl_loader_probe <OpenSSL prefix> [override|invalid-override]');

  ExpectFailure := False;
  ExpectedLibDir := ExpandFileName(
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'lib');
  ExpectedUtilFile := IncludeTrailingPathDelimiter(ExpectedLibDir) +
    'libcrypto.3.dylib';
  ExpectedSSLFile := IncludeTrailingPathDelimiter(ExpectedLibDir) +
    'libssl.3.dylib';
{$IFDEF DARWIN}
  {$IFDEF CPUAARCH64}
  ExpectedFirstUtilFile :=
    '/opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib';
  ExpectedFirstSSLFile := '/opt/homebrew/opt/openssl@3/lib/libssl.3.dylib';
  {$ELSE}
  ExpectedFirstUtilFile := '/usr/local/opt/openssl@3/lib/libcrypto.3.dylib';
  ExpectedFirstSSLFile := '/usr/local/opt/openssl@3/lib/libssl.3.dylib';
  {$ENDIF}
{$ENDIF}
  if ParamCount = 2 then
  begin
    if (ParamStr(2) <> 'override') and
      (ParamStr(2) <> 'invalid-override') then
      Fail('Unknown mode: ' + ParamStr(2));
    DLLUtilName := ExpectedUtilFile;
    if ParamStr(2) = 'invalid-override' then
    begin
      DLLSSLName := ExpectedLibDir + '/missing/libssl.3.dylib';
      ExpectFailure := True;
    end
    else
      DLLSSLName := ExpectedSSLFile;
{$IFDEF DARWIN}
    ExpectedFirstUtilFile := DLLUtilName;
    ExpectedFirstSSLFile := DLLSSLName;
{$ENDIF}
  end;
  ConfiguredUtilName := DLLUtilName;
  ConfiguredSSLName := DLLSSLName;
  Loaded := InitSSLInterface;
{$IFDEF DARWIN}
  if DLLUtilName <> ConfiguredUtilName then
    Fail('Configured libcrypto name changed: ' + DLLUtilName);
  if DLLSSLName <> ConfiguredSSLName then
    Fail('Configured libssl name changed: ' + DLLSSLName);
{$ENDIF}
  if ExpectFailure then
  begin
    if Loaded then
      Fail('Invalid OpenSSL override unexpectedly loaded');
{$IFDEF DARWIN}
    if FirstTriedDLLUtilName <> DLLUtilName then
      Fail('Unexpected first attempted libcrypto: ' + FirstTriedDLLUtilName);
    if FirstTriedDLLSSLName <> DLLSSLName then
      Fail('Unexpected first attempted libssl: ' + FirstTriedDLLSSLName);
    if LastTriedDLLUtilName <> DLLUtilName then
      Fail('Unexpected last attempted libcrypto: ' + LastTriedDLLUtilName);
    if LastTriedDLLSSLName <> DLLSSLName then
      Fail('Unexpected last attempted libssl: ' + LastTriedDLLSSLName);
{$ENDIF}
    if (SSLUtilHandle <> 0) or (SSLLibHandle <> 0) then
      Fail('OpenSSL handles remain after a failed override');
    if InitSSLInterface then
      Fail('Invalid OpenSSL override unexpectedly loaded on retry');
{$IFDEF DARWIN}
    if FirstTriedDLLUtilName <> DLLUtilName then
      Fail('Unexpected first retried libcrypto: ' + FirstTriedDLLUtilName);
    if FirstTriedDLLSSLName <> DLLSSLName then
      Fail('Unexpected first retried libssl: ' + FirstTriedDLLSSLName);
    if LastTriedDLLUtilName <> DLLUtilName then
      Fail('Unexpected last retried libcrypto: ' + LastTriedDLLUtilName);
    if LastTriedDLLSSLName <> DLLSSLName then
      Fail('Unexpected last retried libssl: ' + LastTriedDLLSSLName);
{$ENDIF}
    if (SSLUtilHandle <> 0) or (SSLLibHandle <> 0) then
      Fail('OpenSSL handles remain after a failed override retry');
    Exit;
  end;
  if not Loaded then
    Fail('OpenSSL failed to load');
  if ExpandFileName(ExtractFileDir(SSLUtilFile)) <> ExpectedLibDir then
    Fail('Unexpected libcrypto path: ' + SSLUtilFile);
  if ExpandFileName(ExtractFileDir(SSLLibFile)) <> ExpectedLibDir then
    Fail('Unexpected libssl path: ' + SSLLibFile);
  if ExtractFileName(SSLUtilFile) <> ExtractFileName(ExpectedUtilFile) then
    Fail('Unexpected libcrypto file: ' + SSLUtilFile);
  if ExtractFileName(SSLLibFile) <> ExtractFileName(ExpectedSSLFile) then
    Fail('Unexpected libssl file: ' + SSLLibFile);
{$IFDEF DARWIN}
  if FirstTriedDLLUtilName <> ExpectedFirstUtilFile then
    Fail('Unexpected first attempted libcrypto: ' + FirstTriedDLLUtilName);
  if FirstTriedDLLSSLName <> ExpectedFirstSSLFile then
    Fail('Unexpected first attempted libssl: ' + FirstTriedDLLSSLName);
  if LastTriedDLLUtilName <> SSLUtilFile then
    Fail('Unexpected last attempted libcrypto: ' + LastTriedDLLUtilName);
  if LastTriedDLLSSLName <> SSLLibFile then
    Fail('Unexpected last attempted libssl: ' + LastTriedDLLSSLName);
{$ENDIF}

  VersionText := SSLeayversion(0);
  if Pos('OpenSSL 3', VersionText) <> 1 then
    Fail('Unexpected OpenSSL version: ' + VersionText);
  Method := SslMethodTLS;
  if Method = nil then
    Fail('TLS_method is unavailable');
  Context := SslCtxNew(Method);
  if Context = nil then
    Fail('SSL_CTX_new failed');
  SslCtxFree(Context);
  WriteLn('Loaded ', VersionText);
  WriteLn('libcrypto: ', SSLUtilFile);
  WriteLn('libssl: ', SSLLibFile);

  if not DestroySSLInterface then
    Fail('OpenSSL cleanup failed');
  if (SSLUtilHandle <> 0) or (SSLLibHandle <> 0) then
    Fail('OpenSSL handles remain after cleanup');
  SSLUtilFile := '';
  SSLLibFile := '';
{$IFDEF DARWIN}
  FirstTriedDLLUtilName := 'stale libcrypto path';
  FirstTriedDLLSSLName := 'stale libssl path';
{$ENDIF}
  if not InitSSLInterface then
    Fail('OpenSSL failed to reload');
{$IFDEF DARWIN}
  if (DLLUtilName <> ConfiguredUtilName) or
    (DLLSSLName <> ConfiguredSSLName) then
    Fail('Configured OpenSSL names changed after reload');
  if FirstTriedDLLUtilName <> ExpectedFirstUtilFile then
    Fail('Unexpected first reloaded libcrypto: ' + FirstTriedDLLUtilName);
  if FirstTriedDLLSSLName <> ExpectedFirstSSLFile then
    Fail('Unexpected first reloaded libssl: ' + FirstTriedDLLSSLName);
  if LastTriedDLLUtilName <> SSLUtilFile then
    Fail('Unexpected last reloaded libcrypto: ' + LastTriedDLLUtilName);
  if LastTriedDLLSSLName <> SSLLibFile then
    Fail('Unexpected last reloaded libssl: ' + LastTriedDLLSSLName);
{$ENDIF}
  if ExpandFileName(SSLUtilFile) <> ExpectedUtilFile then
    Fail('Unexpected reloaded libcrypto: ' + SSLUtilFile);
  if ExpandFileName(SSLLibFile) <> ExpectedSSLFile then
    Fail('Unexpected reloaded libssl: ' + SSLLibFile);
  VersionText := SSLeayversion(0);
  if Pos('OpenSSL 3', VersionText) <> 1 then
    Fail('Unexpected reloaded OpenSSL version: ' + VersionText);
  if not DestroySSLInterface then
    Fail('OpenSSL reload cleanup failed');
  if (SSLUtilHandle <> 0) or (SSLLibHandle <> 0) then
    Fail('OpenSSL handles remain after reload cleanup');
end.
