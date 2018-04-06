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
#define AppPublisher "Yury Sidorov & Alexander Petrov"
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
AppCopyright=Copyright (c) 2008-{#CurYear} by Yury Sidorov & Alexander Petrov
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

[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Compact installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "app"; Description: "Main application files"; Types: full compact custom; Flags: fixed
Name: "lang"; Description: "Language files"; Types: full custom

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
Source: "openssl\libeay32.dll"; DestDir: "{app}"; Components: app
Source: "openssl\ssleay32.dll"; DestDir: "{app}"; Components: app

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
#IFDEF UNICODE
  #DEFINE AW "W"
#ELSE
  #DEFINE AW "A"
#ENDIF
type
  INSTALLSTATE = Longint;
const
  INSTALLSTATE_INVALIDARG = -2;  { An invalid parameter was passed to the function. }
  INSTALLSTATE_UNKNOWN = -1;     { The product is neither advertised or installed. }
  INSTALLSTATE_ADVERTISED = 1;   { The product is advertised but not installed. }
  INSTALLSTATE_ABSENT = 2;       { The product is installed for a different user. }
  INSTALLSTATE_DEFAULT = 5;      { The product is installed for the current user. }

  { Visual C++ 2013 Redistributable 12.0.21005 / 12.0.30501.0 }
  VC_2013_REDIST_X86_MIN = '{13A4EE12-23EA-3371-91EE-EFB36DDFFF3E}';

  VC_2013_REDIST_X86_ADD = '{F8CFEB22-A2E7-3971-9EDA-4B11EDEFC185}';

  { Visual C++ 2013 Redistributable 12.0.40660.0 }
  VC_2013_REDIST_X86_MIN_40660 = '{E30D8B21-D82D-3211-82CC-0F0A5D1495E8}';

  VC_2013_REDIST_X86_ADD_40660 = '{7DAD0258-515C-3DD4-8964-BD714199E0F7}';

function MsiQueryProductState(szProduct: string): INSTALLSTATE; 
  external 'MsiQueryProductState{#AW}@msi.dll stdcall';

function VCVersionInstalled(const ProductID: string): Boolean;
begin
  Result := MsiQueryProductState(ProductID) = INSTALLSTATE_DEFAULT;
end;

function VCRedistNeedsInstall: Boolean;
begin
  Result := not ((VCVersionInstalled(VC_2013_REDIST_X86_MIN) and 
    VCVersionInstalled(VC_2013_REDIST_X86_ADD)) or 
    (VCVersionInstalled(VC_2013_REDIST_X86_MIN_40660) and 
    VCVersionInstalled(VC_2013_REDIST_X86_ADD_40660)));
end;


procedure InitializeWizard();
begin
  if VCRedistNeedsInstall then
  begin
    idpAddFileSize('https://download.microsoft.com/download/0/5/6/056dcda9-d667-4e27-8001-8a0c6971d6b1/vcredist_x86.exe', ExpandConstant('{tmp}\vcredist_x86.exe'), 6510544);
    idpAddMirror('https://download.microsoft.com/download/0/5/6/056dcda9-d667-4e27-8001-8a0c6971d6b1/vcredist_x86.exe', 'http://download.microsoft.com/download/0/5/6/056dcda9-d667-4e27-8001-8a0c6971d6b1/vcredist_x86.exe');
  end;
  idpDownloadAfter(wpReady);
end;

[Run]
Filename: "{app}\{#AppExeName}"; Description: "{cm:LaunchProgram,{#AppName}}"; Flags: nowait postinstall skipifsilent
; add the Parameters, WorkingDir and StatusMsg as you wish, just keep here
; the conditional installation Check
Filename: "{tmp}\vcredist_x86"; Parameters: "/q"; Check: VCRedistNeedsInstall

[UninstallDelete]
Type: filesandordirs ; Name: "{localappdata}\{#AppName}"

