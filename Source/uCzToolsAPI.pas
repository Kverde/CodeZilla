unit uCzToolsAPI;

interface

uses
  ToolsAPI, SysUtils, Dialogs, Classes, TypInfo;

type
  TCzToolsAPI = class(TObject)
  strict private
  public
    class function GetModuleInfoFromModule(AModule: IOTAModule): IOTAModuleInfo;
    class function GetProjectFromModule(AModule: IOTAModule): IOTAProject;

    class function GetFormEditor(Module: IOTAModule): IOTAFormEditor;

    class function ModuleIsOpen(AModuleInfo: IOTAModuleInfo): Boolean;

    class function GetStringProperty(AComponent: IOTAComponent; const AIndex: Integer): string;
    class function GetStringPropertyByName(AComponent: IOTAComponent; const APropName: string): string;
  end;




const
  InvalidExpertIndex: integer = -1;

var
  ExpertIndex    : Integer = -1;

  WizardServices : IOTAWizardServices;
  ModuleServices : IOTAModuleServices;
  Services       : INTAServices;



  procedure InitCzToolsAPI(const ABorlandIDEServices: IBorlandIDEServices);

implementation

procedure InitCzToolsAPI(const ABorlandIDEServices: IBorlandIDEServices);
begin
  if not Assigned(BorlandIDEServices) then
    BorlandIDEServices := ABorlandIDEServices;


  WizardServices := BorlandIDEServices as IOTAWizardServices;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Services       := BorlandIDEServices as INTAServices;
end;


{ TToolsAPIWrapper }

class function TCzToolsAPI.GetFormEditor(Module: IOTAModule): IOTAFormEditor;
var
  I: Integer;
  Editor: IOTAEditor;
begin
  for I := 0 to Module.ModuleFileCount - 1 do
  begin
    Editor := Module.ModuleFileEditors[I];
    if Supports(Editor, IOTAFormEditor, Result) then
      Exit;
  end;
  Result := nil;
end;

class function TCzToolsAPI.GetModuleInfoFromModule(
  AModule: IOTAModule): IOTAModuleInfo;
var
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
  i: Integer;
begin
  ProjectGroup := ModuleServices.GetMainProjectGroup;

  if not Assigned(ProjectGroup) then
    Exit(nil);

  for i := 0 to ProjectGroup.ProjectCount - 1 do
  begin
    Project := ProjectGroup.Projects[i];
    if Assigned(Project.FindModuleInfo(AModule.FileName)) then
    begin
      Result := Project.FindModuleInfo(AModule.FileName);
      Exit;
    end;
  end;

  Result := nil;
end;

class function TCzToolsAPI.GetProjectFromModule(
  AModule: IOTAModule): IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
  i: Integer;
begin
  ProjectGroup := ModuleServices.GetMainProjectGroup;

  for i := 0 to ProjectGroup.ProjectCount - 1 do
  begin
    Project := ProjectGroup.Projects[i];
    if Assigned(Project.FindModuleInfo(AModule.FileName)) then
      Exit(Project);
  end;

  Result := nil;
end;

class function TCzToolsAPI.GetStringProperty(AComponent: IOTAComponent;
  const AIndex: Integer): string;
var
  WStr: WideString;
  LStr: AnsiString;
  SStr: ShortString;
begin
  case AComponent.GetPropType(AIndex) of
    tkString  :
      begin
        AComponent.GetPropValue(AIndex, SStr);
        Result := string(SStr);
      end;

    tkLString :
      begin
        AComponent.GetPropValue(AIndex, LStr);
        Result := string(LStr);
      end;

    tkWString :
      begin
        AComponent.GetPropValue(AIndex, WStr);
        Result := WStr;
      end;

    tkUString :
      begin
        AComponent.GetPropValue(AIndex, Result);
      end;
  else
    Result := '';
  end;
end;

class function TCzToolsAPI.GetStringPropertyByName(AComponent: IOTAComponent;
  const APropName: string): string;
var
  WStr: WideString;
  LStr: AnsiString;
  SStr: ShortString;
begin
  case AComponent.GetPropTypeByName(APropName) of
    tkString  :
      begin
        AComponent.GetPropValueByName(APropName, SStr);
        Result := string(SStr);
      end;

    tkLString :
      begin
        AComponent.GetPropValueByName(APropName, LStr);
        Result := string(LStr);
      end;

    tkWString :
      begin
        AComponent.GetPropValueByName(APropName, WStr);
        Result := WStr;
      end;

    tkUString :
      begin
        AComponent.GetPropValueByName(APropName, Result);
      end;
  else
    Result := '';
  end;
end;


class function TCzToolsAPI.ModuleIsOpen(AModuleInfo: IOTAModuleInfo): Boolean;
var
  I: Integer;
begin
  if not Assigned(AModuleInfo) then
    Exit(False);

  for I := 0 to ModuleServices.ModuleCount - 1 do
    if AnsiSameText(ModuleServices.Modules[I].FileName, AModuleInfo.FileName) then
      Exit(True);

  Exit(False);
end;



end.
