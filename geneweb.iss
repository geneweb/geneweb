; GeneWeb InnoSetup Script
; Original script from 2012
; Modernized for GitHub Actions CI in 2026

#define MyAppName "GeneWeb"
; Version is passed via command line: /DMyVersion=X.Y.Z
; Default value for local testing only
#ifndef MyVersion
  #define MyVersion "0.0.0-dev"
#endif
#define MyDistribution "distribution\"
#define MyAppPublisher "GeneWeb Project"
#define MyAppURL "https://geneweb.tuxfamily.org"

[Setup]
; Basic info
AppId={{70C9041C-A917-4AF3-A510-8842CC0FFCC8}
AppName={#MyAppName}
AppVersion={#MyVersion}
AppVerName={#MyAppName} {#MyVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL=https://github.com/geneweb/geneweb/releases

; Installation settings
; Use LocalAppData for write permissions (not Program Files)
DefaultDirName={localappdata}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
DisableDirPage=no
LicenseFile="{#MyDistribution}LICENSE.txt"
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog

; Output settings
OutputDir=.
OutputBaseFilename=GeneWeb-{#MyVersion}-windows-setup

; Compression
Compression=lzma2/ultra64
SolidCompression=yes

; Appearance
WizardStyle=modern
DisableWelcomePage=no
SetupIconFile="{#MyDistribution}gw\images\geneweb.ico"

; Architecture (64-bit only)
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
; Only languages included by default in InnoSetup 6
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Dirs]
Name: "{app}\bases"; Permissions: users-modify

[Files]
; Copy entire distribution
Source: "{#MyDistribution}*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
; Start Menu icons
Name: "{group}\GeneWeb Server (gwd)"; Filename: "{app}\gwd.bat"; WorkingDir: "{app}"; IconFilename: "{app}\gw\images\icone_gwd.ico"; Flags: runminimized; Comment: "Start GeneWeb server"
Name: "{group}\GeneWeb Setup (gwsetup)"; Filename: "{app}\gwsetup.bat"; WorkingDir: "{app}"; IconFilename: "{app}\gw\images\icone_gwsetup.ico"; Flags: runminimized; Comment: "GeneWeb configuration wizard"
Name: "{group}\Getting Started"; Filename: "{app}\START.htm"; Comment: "Open GeneWeb in browser"
Name: "{group}\Bases Folder"; Filename: "{app}\bases"; Comment: "Open databases folder"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

; Desktop icons (optional)
Name: "{autodesktop}\GeneWeb Server"; Filename: "{app}\gwd.bat"; WorkingDir: "{app}"; IconFilename: "{app}\gw\images\icone_gwd.ico"; Tasks: desktopicon; Flags: runminimized; Comment: "Start GeneWeb server"
Name: "{autodesktop}\GeneWeb Setup"; Filename: "{app}\gwsetup.bat"; WorkingDir: "{app}"; IconFilename: "{app}\gw\images\icone_gwsetup.ico"; Tasks: desktopicon; Flags: runminimized; Comment: "GeneWeb configuration"

[Run]
Filename: "{app}\START.htm"; Description: "Open GeneWeb in browser"; Flags: shellexec postinstall skipifsilent nowait unchecked
Filename: "{app}\gwd.bat"; Description: "Start GeneWeb server"; Flags: shellexec postinstall skipifsilent nowait unchecked

[UninstallDelete]
; Clean up logs on uninstall
Type: files; Name: "{app}\gw\*.log"
Type: files; Name: "{app}\*.log"

[CustomMessages]
english.NameAndVersion=%1 version %2
english.AdditionalIcons=Additional icons:
english.CreateDesktopIcon=Create a &desktop icon
english.CreateQuickLaunchIcon=Create a &Quick Launch icon
english.ProgramOnTheWeb=%1 on the Web
english.UninstallProgram=Uninstall %1
english.LaunchProgram=Launch %1
french.NameAndVersion=%1 version %2
french.AdditionalIcons=Icônes supplémentaires :
french.CreateDesktopIcon=Créer une icône sur le &Bureau
french.CreateQuickLaunchIcon=Créer une icône dans la barre de &Lancement rapide
french.ProgramOnTheWeb=Page d’accueil de %1
french.UninstallProgram=Désinstaller %1
french.LaunchProgram=Exécuter %1
german.NameAndVersion=%1 Version %2
german.AdditionalIcons=Zusätzliche Symbole:
german.CreateDesktopIcon=&Desktop-Symbol erstellen
german.CreateQuickLaunchIcon=Symbol in der Schnellstartleiste erstellen
german.ProgramOnTheWeb=%1 im Internet
german.UninstallProgram=%1 entfernen
german.LaunchProgram=%1 starten
spanish.NameAndVersion=%1 versión %2
spanish.AdditionalIcons=Iconos adicionales:
spanish.CreateDesktopIcon=Crear un icono en el &escritorio
spanish.CreateQuickLaunchIcon=Crear un icono de &Inicio Rápido
spanish.ProgramOnTheWeb=%1 en la Web
spanish.UninstallProgram=Desinstalar %1
spanish.LaunchProgram=Ejecutar %1
italian.NameAndVersion=%1 versione %2
italian.AdditionalIcons=Icone aggiuntive:
italian.CreateDesktopIcon=Crea un'icona sul &desktop
italian.CreateQuickLaunchIcon=Crea un'icona nella &barra Avvio veloce
italian.ProgramOnTheWeb=%1 sul Web
italian.UninstallProgram=Disinstalla %1
italian.LaunchProgram=Avvia %1
