unit uCzMain;

interface

uses
  ToolsAPI, Classes, Forms, Dialogs, uCzToolsAPI, Menus, SysUtils,
  uFormCzComponentsSpellChecker;


type
  TCodeZillaWizard = class(TNotifierObject, IOTAWizard)
  strict private
    FMainComponent: TComponent;
    FSettingsPath : string;
    FExpertPath   : string;
  public
    constructor Create;
    destructor Destroy; override;

    // IOTAWIzard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    procedure SpellCheckerMenuClick(Sender: TObject);
  end;

function InitWizard(const BorlandIDEServices : IBorlandIDEServices;
                    RegisterProc : TWizardRegisterProc;
                    var Terminate: TWizardTerminateProc) : Boolean; stdcall;

procedure DoneWizard;

implementation

const
  cExpertFolderName = 'CodeZilla';

procedure DoneWizard;
begin
  if ExpertIndex <> InvalidExpertIndex then
  begin
    WizardServices.RemoveWizard(ExpertIndex);
    ExpertIndex := InvalidExpertIndex;
  end;
end;

function InitWizard(const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; stdcall;
begin
  //http://docwiki.embarcadero.com/Libraries/en/Vcl.Forms.TApplication.Handle
  Application.Handle := (BorlandIDEServices as IOTAServices).GetParentHandle;

  Terminate := DoneWizard;

  InitCzToolsAPI(BorlandIDEServices);

  ExpertIndex := WizardServices.AddWizard(TCodeZillaWizard.Create);

  Result := ExpertIndex <> InvalidExpertIndex;
end;

{ TCodeZillaWizard }

constructor TCodeZillaWizard.Create;
var
  MainMenuItem, MenuItem: TMenuItem;
begin
  FMainComponent := TComponent.Create(nil);

  if Assigned(Services.MainMenu) then
  begin
    MainMenuItem := TMenuItem.Create(FMainComponent);
    MainMenuItem.Caption := 'CodeZilla';
    Services.MainMenu.Items.Insert(Services.MainMenu.Items.Count - 1, MainMenuItem);

    MenuItem := TMenuItem.Create(FMainComponent);
    MenuItem.Caption := 'Spell Checker';
    MenuItem.OnClick := SpellCheckerMenuClick;
    MainMenuItem.Add(MenuItem);
  end;

  FSettingsPath := GetEnvironmentVariable('APPDATA') + PathDelim
                    + cExpertFolderName + PathDelim;

  if not ForceDirectories(FSettingsPath) then
    ShowMessage(Format('Settings directory %s not create', [FSettingsPath]));

  FExpertPath := ExtractFilePath(GetModuleName(HInstance));
end;

destructor TCodeZillaWizard.Destroy;
begin
  FreeAndNil(FMainComponent);

  inherited;
end;

procedure TCodeZillaWizard.Execute;
begin

end;

function TCodeZillaWizard.GetIDString: string;
begin
  Result := 'CodeZillaWizard';
end;

function TCodeZillaWizard.GetName: string;
begin
  Result := 'CodeZilla';
end;

function TCodeZillaWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TCodeZillaWizard.SpellCheckerMenuClick(Sender: TObject);
begin
  TFormCzComponentsSpellChecker.CreateAndShow(FMainComponent, FExpertPath, FSettingsPath);
end;

end.
