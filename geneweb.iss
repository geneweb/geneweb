; Script generated manually for Windows 64 bits
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "GeneWeb"
#define MyVersion GetEnv('PKG_VERSION')
#define MyDistribution "distribution\"
#define MyOutputDir GetEnv('APPVEYOR_BUILD_FOLDER')
#define MyApplowName "geneweb"
#define MyAppShortName "gw"
#define MyAppPublisher "GeneWeb"
#define MyAppURL "https://geneweb.tuxfamily.org/wiki/manual/fr/"
[Setup]

ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
OutputDir={#MyOutputDir}
OutputBaseFilename=geneweb-win64-{#MyVersion}-installer
SetupIconfile="{#MyDistribution}gw\images\icone_gwsetup.ico"

DisableWelcomePage=no
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{70C9041C-A917-4AF3-A510-8842CC0FFCC8}
AppName={#MyAppName}
AppVersion={#MyVersion}
AppVerName={#MyAppName} {#MyVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={userdocs}\{#MyApplowName}-{#MyVersion}
DefaultGroupName={#MyAppName}-{#MyVersion}
LicenseFile="{#MyDistribution}LICENSE.txt"
PrivilegesRequired=admin
Compression=lzma
SolidCompression=yes
WizardStyle=modern

[Files]
Source: "{#MyDistribution}*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\Gwsetup"; Filename: "{app}\gwsetup.bat"; IconFilename: "{app}\gw\images\icone_gwsetup.ico"; Flags: runminimized; Comment: "Daemon Gwsetup {#MyAppName} {#MyVersion}"
Name: "{group}\Gwd"; Filename: "{app}\gwd.bat"; IconFilename: "{app}\gw\images\icone_gwd.ico"; Flags: runminimized; Comment: "Daemon Gwd {#MyAppName} {#MyVersion}"
Name: "{group}\START.htm"; Filename: "{app}\START.htm";
Name: "{group}\{#MyAppName} {#MyVersion}"; Filename: "{app}";
Name: "{group}\{cm:UninstallProgram,{#MyAppName} {#MyVersion}}"; Filename: "{uninstallexe}";

Name: "{commondesktop}\Gwsetup"; Filename: "{app}\gwsetup.bat"; Tasks: desktopicon; IconFilename: "{app}\gw\images\icone_gwsetup.ico"; Flags: runminimized; Comment: "Daemon Gwsetup {#MyAppName} {#MyVersion}"
Name: "{commondesktop}\Gwd"; Filename: "{app}\gwd.bat"; Tasks: desktopicon; IconFilename: "{app}\gw\images\icone_gwd.ico"; Flags: runminimized; Comment: "Daemon Gwd {#MyAppName} {#MyVersion}"
Name: "{commondesktop}\START.htm"; Filename: "{app}\START.htm"; Tasks: desktopicon;

Name: "{userappdata}\Gwsetup"; Filename: "{app}\gwsetup.bat"; Tasks: quicklaunchicon; IconFilename: "{app}\gw\images\icone_gwsetup.ico"; Flags: runminimized; Comment: "Daemon Gwsetup {#MyAppName} {#MyVersion}"
Name: "{userappdata}\Gwd"; Filename: "{app}\gwd.bat"; Tasks: quicklaunchicon; IconFilename: "{app}\gw\images\icone_gwd.ico"; Flags: runminimized; Comment: "Daemon Gwd {#MyAppName} {#MyVersion}"
Name: "{userappdata}\START.htm"; Filename: "{app}\START.htm"; Tasks: quicklaunchicon;

[Run]
Filename: "{app}\gwd.bat"; Description: "{cm:LaunchProgram,Gwd}"; Flags: shellexec postinstall skipifsilent nowait
Filename: "{app}\gwsetup.bat"; Description: "{cm:LaunchProgram,Gwsetup}"; Flags: shellexec postinstall skipifsilent nowait
Filename: "{app}\START.htm"; Description: "{cm:LaunchProgram,START.htm}"; Flags: shellexec postinstall skipifsilent nowait

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}";
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; OnlyBelowVersion: 0.0,6.01;

[CustomMessages]
english.NameAndVersion=%1 version %2
english.AdditionalIcons=Additional icons:
english.CreateDesktopIcon=Create a &desktop icon
english.CreateQuickLaunchIcon=Create a &Quick Launch icon
english.ProgramOnTheWeb=%1 on the Web
english.UninstallProgram=Uninstall %1
english.LaunchProgram=Launch %1
english.AssocFileExtension=&Associate %1 with the %2 file extension
english.AssocingFileExtension=Associating %1 with the %2 file extension...
english.AutoStartProgramGroupDescription=Startup:
english.AutoStartProgram=Automatically start %1
english.AddonHostProgramNotFound=%1 could not be located in the folder you selected.%n%nDo you want to continue anyway?
brazilianportuguese.NameAndVersion=%1 versão %2
brazilianportuguese.AdditionalIcons=Ícones adicionais:
brazilianportuguese.CreateDesktopIcon=Criar um ícone na Área de &Trabalho
brazilianportuguese.CreateQuickLaunchIcon=Criar um ícone na &Barra de Inicialização Rápida
brazilianportuguese.ProgramOnTheWeb=%1 na Internet
brazilianportuguese.UninstallProgram=Desinstalar %1
brazilianportuguese.LaunchProgram=Executar %1
brazilianportuguese.AssocFileExtension=Associar %1 com a e&xtensão de arquivo %2
brazilianportuguese.AssocingFileExtension=Associando %1 com a extensão de arquivo...
brazilianportuguese.AutoStartProgramGroupDescription=Startup:
brazilianportuguese.AutoStartProgram=Iniciar automaticamente %1
brazilianportuguese.AddonHostProgramNotFound=%1 não pôde ser localizado na pasta que você selecionou.%n%nVocê deseja continuar assim mesmo?
danish.NameAndVersion=%1 version %2
danish.AdditionalIcons=Ekstra ikoner:
danish.CreateDesktopIcon=Lav ikon på skrive&bordet
danish.CreateQuickLaunchIcon=Lav &hurtigstart-ikon
danish.ProgramOnTheWeb=%1 på internettet
danish.UninstallProgram=Afinstaller (fjern) %1
danish.LaunchProgram=&Kør %1
danish.AssocFileExtension=Sammen&kæd %1 med filtypen %2
danish.AssocingFileExtension=Sammenkæder %1 med filtypen %2...
danish.AutoStartProgramGroupDescription=Start:
danish.AutoStartProgram=Start automatisk %1
danish.AddonHostProgramNotFound=%1 blev ikke fundet i den mappe du angav.%n%nØnsker du alligevel at fortsætte?
dutch.NameAndVersion=%1 versie %2
dutch.AdditionalIcons=Extra snelkoppelingen:
dutch.CreateDesktopIcon=Maak een snelkoppeling op het &bureaublad
dutch.CreateQuickLaunchIcon=Maak een snelkoppeling op de &Snel starten werkbalk
dutch.ProgramOnTheWeb=%1 op het Web
dutch.UninstallProgram=Verwijder %1
dutch.LaunchProgram=&Start %1
dutch.AssocFileExtension=&Koppel %1 aan de %2 bestandsextensie
dutch.AssocingFileExtension=Bezig met koppelen van %1 aan de %2 bestandsextensie...
dutch.AutoStartProgramGroupDescription=Opstarten:
dutch.AutoStartProgram=%1 automatisch starten
dutch.AddonHostProgramNotFound=%1 kon niet worden gevonden in de geselecteerde map.%n%nWilt u toch doorgaan?
finnish.NameAndVersion=%1 versio %2
finnish.AdditionalIcons=Lisäkuvakkeet:
finnish.CreateDesktopIcon=Lu&o kuvake työpöydälle
finnish.CreateQuickLaunchIcon=Luo kuvake &pikakäynnistyspalkkiin
finnish.ProgramOnTheWeb=%1 Internetissä
finnish.UninstallProgram=Poista %1
finnish.LaunchProgram=&Käynnistä %1
finnish.AssocFileExtension=&Yhdistä %1 tiedostopäätteeseen %2
finnish.AssocingFileExtension=Yhdistetään %1 tiedostopäätteeseen %2 ...
finnish.AutoStartProgramGroupDescription=Käynnistys:
finnish.AutoStartProgram=Käynnistä %1 automaattisesti
finnish.AddonHostProgramNotFound=%1 ei ole valitsemassasi kansiossa.%n%nHaluatko jatkaa tästä huolimatta?
french.NameAndVersion=%1 version %2
french.AdditionalIcons=Icônes supplémentaires :
french.CreateDesktopIcon=Créer une icône sur le &Bureau
french.CreateQuickLaunchIcon=Créer une icône dans la barre de &Lancement rapide
french.ProgramOnTheWeb=Page d'accueil de %1
french.UninstallProgram=Désinstaller %1
french.LaunchProgram=Exécuter %1
french.AssocFileExtension=&Associer %1 avec l'extension de fichier %2
french.AssocingFileExtension=Associe %1 avec l'extension de fichier %2...
french.AutoStartProgramGroupDescription=Démarrage :
french.AutoStartProgram=Démarrer automatiquement %1
french.AddonHostProgramNotFound=%1 n'a pas été trouvé dans le dossier que vous avez choisi.%n%nVoulez-vous continuer malgré tout ?
german.NameAndVersion=%1 Version %2
german.AdditionalIcons=Zusätzliche Symbole:
german.CreateDesktopIcon=&Desktop-Symbol erstellen
german.CreateQuickLaunchIcon=Symbol in der Schnellstartleiste erstellen
german.ProgramOnTheWeb=%1 im Internet
german.UninstallProgram=%1 entfernen
german.LaunchProgram=%1 starten
german.AssocFileExtension=&Registriere %1 mit der %2-Dateierweiterung
german.AssocingFileExtension=%1 wird mit der %2-Dateierweiterung registriert...
german.AutoStartProgramGroupDescription=Beginn des Setups:
german.AutoStartProgram=Starte automatisch%1
german.AddonHostProgramNotFound=%1 konnte im ausgewählten Ordner nicht gefunden werden.%n%nMöchten Sie dennoch fortfahren?
hebrew.NameAndVersion=%1 גירסה %2
hebrew.AdditionalIcons=סימלונים נוספים:
hebrew.CreateDesktopIcon=צור קיצור דרך על &שולחן העבודה
hebrew.CreateQuickLaunchIcon=צור סימלון בשורת ההרצה המהירה
hebrew.ProgramOnTheWeb=%1 ברשת
hebrew.UninstallProgram=הסר את %1
hebrew.LaunchProgram=הפעל %1
hebrew.AssocFileExtension=&קשר את %1 עם סיומת הקובץ %2
hebrew.AssocingFileExtension=מקשר את %1 עם סיומת הקובץ %2
hebrew.AutoStartProgramGroupDescription=הפעלה אוטומטית:
hebrew.AutoStartProgram=הפעל אוטומטית %1
hebrew.AddonHostProgramNotFound=%1 לא נמצא בתיקיה שבחרת.%n%nאתה רוצה להמשיך בכל זאת?
italian.NameAndVersion=%1 versione %2
italian.AdditionalIcons=Icone aggiuntive:
italian.CreateDesktopIcon=Crea un'icona sul &desktop
italian.CreateQuickLaunchIcon=Crea un'icona nella &barra Avvio veloce
italian.ProgramOnTheWeb=%1 sul Web
italian.UninstallProgram=Disinstalla %1
italian.LaunchProgram=Avvia %1
italian.AssocFileExtension=&Associa l'estensione %2 a %1
italian.AssocingFileExtension=Associazione dell'estensione %2 a %1 in corso...
italian.AutoStartProgramGroupDescription=Avvio automatico:
italian.AutoStartProgram=Avvia automaticamente %1
italian.AddonHostProgramNotFound=Impossibile individuare %1 nella cartella selezionata.%n%nProseguire ugualmente?
norwegian.NameAndVersion=%1 versjon %2
norwegian.AdditionalIcons=Ekstra-ikoner:
norwegian.CreateDesktopIcon=Lag ikon på &skrivebordet
norwegian.CreateQuickLaunchIcon=Lag et &Hurtigstarts-ikon
norwegian.ProgramOnTheWeb=%1 på nettet
norwegian.UninstallProgram=Avinstaller %1
norwegian.LaunchProgram=Kjør %1
norwegian.AssocFileExtension=&Koble %1 med filetternavnet %2
norwegian.AssocingFileExtension=Kobler %1 med filetternavnet %2...
norwegian.AutoStartProgramGroupDescription=Oppstart:
norwegian.AutoStartProgram=Start %1 automatisk
norwegian.AddonHostProgramNotFound=%1 ble ikke funnet i katalogen du valgte.%n%nVil du fortsette likevel?
polish.NameAndVersion=%1 wersja %2
polish.AdditionalIcons=Dodatkowe ikony:
polish.CreateDesktopIcon=Utwórz ikonę na &pulpicie
polish.CreateQuickLaunchIcon=Utwórz ikonę na pasku &szybkiego uruchamiania
polish.ProgramOnTheWeb=Strona WWW programu %1
polish.UninstallProgram=Deinstalacja programu %1
polish.LaunchProgram=Uruchom program %1
polish.AssocFileExtension=&Przypisz program %1 do rozszerzenia pliku %2
polish.AssocingFileExtension=Przypisywanie programu %1 do rozszerzenia pliku %2...
polish.AutoStartProgramGroupDescription=Autostart:
polish.AutoStartProgram=Automatycznie uruchamiaj %1
polish.AddonHostProgramNotFound=%1 nie został znaleziony we wskazanym przez Ciebie folderze.%n%nCzy pomimo tego chcesz kontynuować?
portuguese.NameAndVersion=%1 versão %2
portuguese.AdditionalIcons=Ícones adicionais:
portuguese.CreateDesktopIcon=Criar ícone no Ambiente de &Trabalho
portuguese.CreateQuickLaunchIcon=&Criar ícone na barra de Iniciação Rápida
portuguese.ProgramOnTheWeb=%1 na Web
portuguese.UninstallProgram=Desinstalar o %1
portuguese.LaunchProgram=Executar o %1
portuguese.AssocFileExtension=Associa&r o %1 aos ficheiros com a extensão %2
portuguese.AssocingFileExtension=A associar o %1 aos ficheiros com a extensão %2...
portuguese.AutoStartProgramGroupDescription=Inicialização Automática:
portuguese.AutoStartProgram=Iniciar %1 automaticamente
portuguese.AddonHostProgramNotFound=Não foi possível localizar %1 na pasta seleccionada.%n%nDeseja continuar de qualquer forma?
russian.NameAndVersion=%1, версия %2
russian.AdditionalIcons=Дополнительные значки:
russian.CreateDesktopIcon=Создать значок на &Рабочем столе
russian.CreateQuickLaunchIcon=Создать значок в &Панели быстрого запуска
russian.ProgramOnTheWeb=Сайт %1 в Интернете
russian.UninstallProgram=Деинсталлировать %1
russian.LaunchProgram=Запустить %1
russian.AssocFileExtension=Св&язать %1 с файлами, имеющими расширение %2
russian.AssocingFileExtension=Связывание %1 с файлами %2...
russian.AutoStartProgramGroupDescription=Автозапуск:
russian.AutoStartProgram=Автоматически запускать %1
russian.AddonHostProgramNotFound=%1 не найден в указанной вами папке.%n%nВы всё равно хотите продолжить?
spanish.NameAndVersion=%1 versión %2
spanish.AdditionalIcons=Iconos adicionales:
spanish.CreateDesktopIcon=Crear un icono en el &escritorio
spanish.CreateQuickLaunchIcon=Crear un icono de &Inicio Rápido
spanish.ProgramOnTheWeb=%1 en la Web
spanish.UninstallProgram=Desinstalar %1
spanish.LaunchProgram=Ejecutar %1
spanish.AssocFileExtension=&Asociar %1 con la extensión de archivo %2
spanish.AssocingFileExtension=Asociando %1 con la extensión de archivo %2...
spanish.AutoStartProgramGroupDescription=Inicio:
spanish.AutoStartProgram=Iniciar automáticamente %1
spanish.AddonHostProgramNotFound=%1 no pudo ser localizado en la carpeta seleccionada.%n%n¿Desea continuar de todas formas?

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "armenian"; MessagesFile: "compiler:Languages\Armenian.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "catalan"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "corsican"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "finnish"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "icelandic"; MessagesFile: "compiler:Languages\Icelandic.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "slovak"; MessagesFile: "compiler:Languages\Slovak.isl"
Name: "slovenian"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "turkish"; MessagesFile: "compiler:Languages\Turkish.isl"
Name: "ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl"
