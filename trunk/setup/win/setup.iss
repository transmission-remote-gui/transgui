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
#define AppPublisher "Yury Sidorov"
#define AppURL "http://code.google.com/p/transmisson-remote-gui/"
#define AppExeName "transgui.exe"
#define CurYear GetDateTimeString('yyyy', '', '')

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

[Setup]
AppId=transgui
AppName={#AppName}
AppVerName={#AppVerName}
AppCopyright=Copyright (c) 2008-{#CurYear} by Yury Sidorov
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
LicenseFile=..\..\LICENSE.txt
InfoAfterFile=..\..\history.txt
OutputDir=..\..\Release
OutputBaseFilename=transgui-{#AppVersion}-setup

Compression=lzma/max
InternalCompressLevel=max
SolidCompression=no

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
Source: "..\..\LICENSE.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\readme.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\history.txt"; DestDir: "{app}"; Flags: ignoreversion; Components: app
Source: "..\..\lang\transgui.*"; DestDir: "{app}\lang"; Flags: ignoreversion; Components: lang

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppExeName}"; WorkingDir: {app}
Name: "{group}\History"; Filename: "{app}\history.txt"; WorkingDir: {app}
Name: "{group}\Read me"; Filename: "{app}\readme.txt"; WorkingDir: {app}
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

[Run]
Filename: "{app}\{#AppExeName}"; Description: "{cm:LaunchProgram,{#AppName}}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs ; Name: "{localappdata}\{#AppName}"

