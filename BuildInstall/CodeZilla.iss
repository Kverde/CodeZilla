
#DEFINE VERSION "0.1"

#define Product "CodeZilla"

#ifdef RS2009
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "2009"
  #define DLLSuffix    "D2009"
  #define IDERegName   "BDS"
  #define IDERegVer    "6"
  #define RegCompany   "CodeGear"
#endif
#ifdef RS2010
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "2010"
  #define DLLSuffix    "D2010"
  #define IDERegName   "BDS"
  #define IDERegVer    "7"
  #define RegCompany   "CodeGear"
#endif
#ifdef RSXE1
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE1"
  #define DLLSuffix    "XE1"
  #define IDERegName   "BDS"
  #define IDERegVer    "8"
  #define RegCompany   "Embarcadero"
#endif
#ifdef RSXE2
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE2"
  #define DLLSuffix    "XE2"
  #define IDERegName   "BDS"
  #define IDERegVer    "9"
  #define RegCompany   "Embarcadero"
#endif
#ifdef RSXE3
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE3"
  #define DLLSuffix    "XE3"
  #define IDERegName   "BDS"
  #define IDERegVer    "10"
  #define RegCompany   "Embarcadero"
#endif
#ifdef RSXE4
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE4"
  #define DLLSuffix    "XE4"
  #define IDERegName   "BDS"
  #define IDERegVer    "11"
  #define RegCompany   "Embarcadero"
#endif

#define FullName    Product +" for "+ IDELongName + " " + IDEVer
#define BinaryDir   IDEShortName + IDEVer
#define DLLName     "CodeZillaDLL_" + DLLSuffix + ".dll"
#define AppIDValue  Product + IDEShortName + IDEVer
#define ThisYear    GetDateTimeString('yyyy', '', '');

[Setup]
AppCopyright=Copyright 2013-{#ThisYear} by {#Product} Development Team
AppName={#Product}
AppVerName={#FullName}
AppID={#AppIDValue}
DefaultDirName={pf}\{#FullName}
DefaultGroupName={#FullName}
;LicenseFile=..\Documentation\License.txt
;InfoBeforeFile=..\Documentation\PreInstall.txt
AppPublisher={#Product} Development Team
AppPublisherURL=https://github.com/Kverde/CodeZilla
AppVersion={#Version}
AppMutex=CodeZilla.Addin.For.Borland.IDEs
;UninstallDisplayIcon={app}\ExpertManager.exe
VersionInfoVersion={#Version}
VersionInfoDescription={#FullName} Setup
VersionInfoTextVersion={#Version}
SolidCompression=yes
OutputBaseFilename=SetupCodeZilla_{#DLLSuffix}
OutputDir=..\Setup 

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Files]
Source: ..\Bin\{#DLLName}; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\hunspell.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\Bin\Dict\*; DestDir: {app}\Dict; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]


[Registry]
Root: HKCU; Subkey: Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts; ValueType: STRING; ValueName: CodeZilla; ValueData: {app}\{#DLLName}; Flags: uninsdeletevalue; Check: IDEExecuted
Root: HKLM; Subkey: Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts; ValueType: STRING; ValueName: CodeZilla; ValueData: {app}\{#DLLName}; Flags: uninsdeletevalue uninsdeletekeyifempty

[Code]

procedure DeleteDirAndSettings(Dir: string);
begin
  if (Dir <> '') and DirExists(Dir) then
  begin
    Dir := AddBackslash(Dir);
    Deltree(Dir + '*.*', False, True, False);
	
    // Delete this settings directory itself only if it is now empty
    Deltree(Dir, True, False, False);
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usPostUninstall then
  begin
    // Delete the DLL reference even if it was copied from the HKLM tree
    RegDeleteValue(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts', 'CodeZilla');
    case MsgBox('Do you want to delete all of your Components Spell Checker preferences and data files?',
                mbConfirmation, MB_YESNOCANCEL or MB_DEFBUTTON2) of
      IDYES:
        begin
          DeleteDirAndSettings(ExpandConstant('{app}'));
        end;
      IDCANCEL:
        Abort;
    end;
  end;
end;

function IDEExecuted: Boolean;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0');
end;


