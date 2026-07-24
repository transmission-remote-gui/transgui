#ifndef AppVersion
  #define AppVersion GetFileVersion(SourcePath+'..\..\transgui.exe')
  #define AppVersion Copy(AppVersion, 1, RPos('.', AppVersion) - 1)
  #define tmpvar Copy(AppVersion, RPos('.', AppVersion) + 1, 3)
  #if tmpvar == "0"
    #define AppVersion Copy(AppVersion, 1, RPos('.', AppVersion) - 1)
  #endif
  #undef tmpvar
;  #define AppVersion AppVersion+'-beta'
#endif

#define AppName "Transmission Remote GUI"
#define AppVerName AppName + " " + AppVersion
#define AppPublisher "Yury Sidorov & Transmission Remote GUI working group"
#define AppURL "https://github.com/transmission-remote-gui/transgui"
#define AppExeName "transgui.exe"
#define CurYear GetDateTimeString('yyyy', '', '')
#include <idp.iss>

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "pt_br"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "no"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "hy"; MessagesFile: "compiler:Languages\Armenian.islu"
Name: "el"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "gd"; MessagesFile: "compiler:Languages\ScottishGaelic.isl"
Name: "he"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "sr"; MessagesFile: "compiler:Languages\SerbianCyrillic.isl"
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "sr"; MessagesFile: "compiler:Languages\SerbianLatin.isl"
Name: "co"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "ja"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "ne"; MessagesFile: "compiler:Languages\Nepali.islu"
Name: "tr"; MessagesFile: "compiler:Languages\Turkish.isl"
Name: "uk"; MessagesFile: "compiler:Languages\Ukrainian.isl"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "zh_tw"; MessagesFile: "..\..\Inno Setup lang\ChineseTraditional.isl"
Name: "zh_cn"; MessagesFile: "..\..\Inno Setup lang\ChineseSimplified.isl"


[Setup]
AppId=transgui
AppName={#AppName}
AppVerName={#AppVerName}
AppCopyright=Copyright (c) 2008-{#CurYear} by Yury Sidorov & Transmission Remote GUI working group
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}
AppUpdatesURL={#AppURL}
UninstallDisplayIcon={app}\transgui.exe

VersionInfoVersion={#GetFileVersion(SourcePath+'..\..\transgui.exe')}
VersionInfoTextVersion={#GetFileVersion(SourcePath+'..\..\transgui.exe')}

DefaultDirName={pf}\{#AppName}
DefaultGroupName={#AppName}
AllowNoIcons=yes
LicenseFile=..\..\LICENSE
InfoAfterFile=..\..\history.txt
OutputDir=..\..\Release
OutputBaseFilename=transgui-{#AppVersion}-setup

Compression=lzma2/ultra64
InternalCompressLevel=ultra64
SolidCompression=yes

PrivilegesRequired=poweruser
ChangesAssociations=yes

WizardImageFile=compiler:\WizModernImage-IS.bmp
WizardSmallImageFile=compiler:\WizModernSmallImage-IS.bmp

LanguageDetectionMethod=locale
ShowLanguageDialog=yes

#if GetEnv("CODECERT") != ""
#define CODECERT GetEnv("CODECERT")
SignTool=signtool sign /d "{#AppName} Setup" /du "{#AppURL}" /f "{#CODECERT}" /v $f
#endif

[Components]
Name: "app"; Description: "Main application files"; Types: full compact custom; Flags: fixed
Name: "lang"; Description: "Language files"; Types: full custom
Name: "openssl"; Description: "OpenSSL files"; Types: full custom

[Tasks]
Name: regfileext; Description: "{cm:AssocFileExtension,{#AppName},.torrent}"; Flags: unchecked
Name: regmagnet; Description: "Handle magnet links by {#AppName}"; Flags: unchecked
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\transgui.exe"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\LICENSE"; DestDir: "{app}"; DestName: "LICENSE.txt"; Flags: ignoreversion; Components: app
Source: "..\..\README.md"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\README.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\history.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\lang\transgui.*"; DestDir: "{app}\lang"; Flags: ignoreversion; Components: lang
; OpenSSL
Source: "openssl\libcrypto-3.dll"; DestDir: "{app}"; Components: openssl
Source: "openssl\libssl-3.dll"; DestDir: "{app}"; Components: openssl

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: {app}
Name: "{group}\History"; Filename: "{app}\history.txt"; WorkingDir: {app}
Name: "{group}\Read me"; Filename: "{app}\README.md"; WorkingDir: {app}
Name: "{group}\Read me"; Filename: "{app}\README.txt"; WorkingDir: {app}
Name: "{group}\License"; Filename: "{app}\LICENSE.txt"; WorkingDir: {app}
Name: "{group}\Home page"; Filename: "{#AppURL}"; WorkingDir: {app}
Name: "{group}\{cm:UninstallProgram,{#AppName}}"; Filename: "{uninstallexe}"; WorkingDir: {app}
Name: "{userdesktop}\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: {app}; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: {app}; Tasks: quicklaunchicon

[Registry]
Root: HKCU; Subkey: "Software\Classes\.torrent"; ValueType: string; ValueName: ""; ValueData: "{#AppName}"; Flags: uninsdeletevalue; Tasks: regfileext
Root: HKCU; Subkey: "Software\Classes\{#AppName}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"",0"; Flags: uninsdeletevalue; Tasks: regfileext
Root: HKCU; Subkey: "Software\Classes\{#AppName}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"" ""%1"""; Flags: uninsdeletevalue; Tasks: regfileext

Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: ""; ValueData: "Magnet URI"; Flags: createvalueifdoesntexist uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: "Content Type"; ValueData: "application/x-magnet"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet"; ValueType: string; ValueName: "URL Protocol"; ValueData: ""; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"",0"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\shell"; ValueType: string; ValueName: ""; ValueData: "open"; Flags: uninsdeletevalue; Tasks: regmagnet
Root: HKCU; Subkey: "Software\Classes\Magnet\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"" ""%1"""; Flags: uninsdeletevalue; Tasks: regmagnet

[Code]
const
  VC_REDIST_MIN_MAJOR = 14;
  VC_REDIST_MIN_MINOR = 44;
  VC_REDIST_MIN_BUILD = 35211;
  VC_RUNTIME_KEY = 'SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\X86';

function VCVersionInstalled(const RootKey: Integer): Boolean;
var
  Installed: Cardinal;
  Major, Minor, Bld: Cardinal;
begin
  Result := RegQueryDWordValue(RootKey, VC_RUNTIME_KEY, 'Installed', Installed) and
    (Installed = 1) and
    RegQueryDWordValue(RootKey, VC_RUNTIME_KEY, 'Major', Major) and
    RegQueryDWordValue(RootKey, VC_RUNTIME_KEY, 'Minor', Minor) and
    RegQueryDWordValue(RootKey, VC_RUNTIME_KEY, 'Bld', Bld) and
    ((Major > VC_REDIST_MIN_MAJOR) or
     ((Major = VC_REDIST_MIN_MAJOR) and (Minor > VC_REDIST_MIN_MINOR)) or
     ((Major = VC_REDIST_MIN_MAJOR) and (Minor = VC_REDIST_MIN_MINOR) and
      (Bld >= VC_REDIST_MIN_BUILD)));
end;

function VCRedistNeedsInstall: Boolean;
begin
  Result := not VCVersionInstalled(HKLM32);
end;

function IsExistingInstallation: Boolean;
begin
  Result := FileExists(ExpandConstant('{app}\{#AppExeName}'));
end;

function QuotePowerShellString(const Value: String): String;
begin
  Result := Value;
  StringChangeEx(Result, '''', '''''', True);
  Result := '''' + Result + '''';
end;

function VerifyVCRedistSignature(const FileName: String;
  var ResultCode: Integer): Boolean;
var
  PowerShellCommand: String;
  PowerShellStarted: Boolean;
begin
  PowerShellCommand :=
    '$signature = Microsoft.PowerShell.Security\Get-AuthenticodeSignature ' +
    '-LiteralPath ' + QuotePowerShellString(FileName) + ' -ErrorAction Stop; ' +
    'if ($null -eq $signature.SignerCertificate) { exit 11 }; ' +
    'if ($signature.Status -ne ' +
    '[System.Management.Automation.SignatureStatus]::Valid) { exit 12 }; ' +
    '$publisher = $signature.SignerCertificate.GetNameInfo(' +
    '[System.Security.Cryptography.X509Certificates.X509NameType]::SimpleName, ' +
    '$false); ' +
    'if (-not [string]::Equals($publisher, ''Microsoft Corporation'', ' +
    '[System.StringComparison]::Ordinal)) { exit 13 }; exit 0';
  PowerShellCommand := 'try { ' + PowerShellCommand +
    ' } catch { exit 10 }';

  PowerShellStarted := Exec(
    ExpandConstant('{sys}\WindowsPowerShell\v1.0\powershell.exe'),
    '-NoLogo -NoProfile -NonInteractive -Command ' + PowerShellCommand,
    '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  if not PowerShellStarted then
    Log(Format('Could not start Authenticode verification (error %d: %s).', [
      ResultCode, SysErrorMessage(ResultCode)]))
  else if ResultCode <> 0 then
    Log(Format('Authenticode verification failed with exit code %d.', [ResultCode]));

  Result := PowerShellStarted and (ResultCode = 0);
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  FileName: String;
  ResultCode: Integer;
begin
  Result := '';
  if not (VCRedistNeedsInstall and IsComponentSelected('openssl')) then
    Exit;

  FileName := ExpandConstant('{tmp}\vc_redist.x86.exe');
  if not FileExists(FileName) then
  begin
    Log('The downloaded Visual C++ runtime installer is missing.');
    Result := 'Setup could not verify the downloaded Microsoft Visual C++ ' +
      'Runtime because the file is missing. Cancel Setup and try again, or ' +
      'install the runtime manually from Microsoft.';
    Exit;
  end;

  if not VerifyVCRedistSignature(FileName, ResultCode) then
    Result := 'Setup could not verify that the downloaded Microsoft Visual ' +
      'C++ Runtime is authentic. Cancel Setup and try again, or install the ' +
      'runtime manually from Microsoft.';
end;


procedure InitializeWizard();
begin
  if VCRedistNeedsInstall then
    idpAddFileComp('https://aka.ms/vs/17/release/vc_redist.x86.exe', ExpandConstant('{tmp}\vc_redist.x86.exe'), 'openssl');
  idpDownloadAfter(wpReady);
end;

[InstallDelete]
Type: files; Name: "{app}\libcrypto-1_1.dll"; Components: openssl; Check: IsExistingInstallation
Type: files; Name: "{app}\libssl-1_1.dll"; Components: openssl; Check: IsExistingInstallation

[Run]
Filename: "{app}\{#AppExeName}"; Description: "{cm:LaunchProgram,{#AppName}}"; Flags: nowait postinstall skipifsilent
; add the Parameters, WorkingDir and StatusMsg as you wish, just keep here
; the conditional installation Check
Filename: "{tmp}\vc_redist.x86.exe"; Parameters: "/install /quiet /norestart"; Check: VCRedistNeedsInstall and IsComponentSelected('openssl')

[UninstallDelete]
Type: filesandordirs ; Name: "{localappdata}\{#AppName}"
