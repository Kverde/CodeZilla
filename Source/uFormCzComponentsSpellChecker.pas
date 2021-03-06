unit uFormCzComponentsSpellChecker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList, StdCtrls, ExtCtrls,
  uCzSpellChecker, ToolsAPI, uCzToolsAPI, uFormCzResult, Generics.Collections,
  StdActns, ActnList, uCzGeneral, uFormCzWait;

type
  TIgnoreList = TList<string>;
  TIgnoreDict = TDictionary<string, string>;


  TFormCzComponentsSpellChecker = class(TForm)
    Panel3: TPanel;
    rgKindFind: TRadioGroup;
    bDictionary: TButton;
    Panel1: TPanel;
    mIgnoreList: TMemo;
    bOk: TButton;
    bCancel: TButton;
    bHelp: TButton;
    bRefresh: TButton;
    bSelectDictionary: TButton;
    Label1: TLabel;
    lbCurrentDictionary: TLinkLabel;
    alEdit: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    procedure FormDestroy(Sender: TObject);
    procedure bSelectDictionaryClick(Sender: TObject);
    procedure bDictionaryClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
  private
    FIgnoreListFileName: string;
    FSpellChecker: TSpellChecker;

    FFormResult: TFormCzResult;

    FIgnoreComponentType: TIgnoreList;
    FIgnoreProperty: TIgnoreList;
    FIgnoreComponentName: TIgnoreList;
    FIgnoreCompProp: TIgnoreDict;

    procedure UpdateIgnore;

    procedure SetDictionaryName(const ADictionaryName: string);

    procedure FindInProject(AProject : IOTAProject);

    procedure FindInCurrentForm;
    procedure FindInOpenForms;
    procedure FindInCurrentProject;
    procedure FindInProjectGroup;

    procedure SpellCheckModule(AModule: IOTAModule);

  public
    constructor Create(AOwner: TComponent;
                       const AExpertPath, ASettingsPath: string); reintroduce;

    class procedure CreateAndShow(const AOwner: TComponent;
                                  const AExpertPath, ASettingPath: string);


  end;

implementation

{$R *.dfm}

uses
  TypInfo;

const
  cIgnoreListFileName = 'IgnoreList.txt';
  cFormCaption = 'Components spell checker';

var
  Form: TFormCzComponentsSpellChecker = nil;


{ TFormCzComponentsSpellChecker }

procedure TFormCzComponentsSpellChecker.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormCzComponentsSpellChecker.bDictionaryClick(Sender: TObject);
begin
  FSpellChecker.ShowCustomWordFile;
end;

procedure TFormCzComponentsSpellChecker.bOkClick(Sender: TObject);
begin
  try
    UpdateIgnore;

    FFormResult.ClearResults;

    case rgKindFind.ItemIndex of
      0: FindInCurrentForm;
      1: FindInOpenForms;
      2: FindInCurrentProject;
      3: FindInProjectGroup;
    end;
  finally
    Caption := cFormCaption;
  end;

  FFormResult.Show;
  Close;
end;

procedure TFormCzComponentsSpellChecker.bRefreshClick(Sender: TObject);
var
  Wait: IWait;
begin

  Wait := TWait.Create(Self);

  Sleep(3000);

  FSpellChecker.RefreshCustomWord;
end;

procedure TFormCzComponentsSpellChecker.bSelectDictionaryClick(Sender: TObject);
var
  Wait: IWait;
begin
  FSpellChecker.ChoiceDictionary;

  Wait := TWait.Create(Self);

  SetDictionaryName(FSpellChecker.DictionaryName);
end;

constructor TFormCzComponentsSpellChecker.Create(AOwner: TComponent;
  const AExpertPath, ASettingsPath: string);
begin
  inherited Create(AOwner);

  FIgnoreListFileName := ASettingsPath + cIgnoreListFileName;

  FSpellChecker := TSpellChecker.Create(AExpertPath, ASettingsPath);

  FFormResult := TFormCzResult.Create(Self, FSpellChecker, mIgnoreList.Lines, Self);

  FIgnoreComponentType := TIgnoreList.Create;
  FIgnoreComponentName := TIgnoreList.Create;

  FIgnoreProperty  := TIgnoreList.Create;

  FIgnoreCompProp  := TIgnoreDict.Create;


  if FileExists(FIgnoreListFileName) then
    mIgnoreList.Lines.LoadFromFile(FIgnoreListFileName);

  SetDictionaryName(FSpellChecker.DictionaryName);
end;

class procedure TFormCzComponentsSpellChecker.CreateAndShow(
  const AOwner: TComponent; const AExpertPath, ASettingPath: string);
begin
  if not Assigned(Form) then
    Form := TFormCzComponentsSpellChecker.Create(AOwner, AExpertPath, ASettingPath);

  Form.Show;
end;

procedure TFormCzComponentsSpellChecker.FindInCurrentForm;
begin
  if Assigned(ModuleServices.CurrentModule) then
    SpellCheckModule(ModuleServices.CurrentModule);
end;

procedure TFormCzComponentsSpellChecker.FindInCurrentProject;
begin
  if Assigned(ModuleServices.GetActiveProject) then
    FindInProject(ModuleServices.GetActiveProject);
end;

procedure TFormCzComponentsSpellChecker.FindInOpenForms;
var
  i: Integer;
  Wait: IWaitForm;
begin
  Wait := TWaitForm.Create(Self, ModuleServices.ModuleCount);

  for i := 0 to ModuleServices.ModuleCount - 1 do
    begin
      SpellCheckModule(ModuleServices.Modules[i]);

      Wait.IncProgress;
    end;
end;

procedure TFormCzComponentsSpellChecker.FindInProject(AProject: IOTAProject);

  function PasFrmModule(pModule : IOTAModuleInfo) : Boolean;
  begin
    Result := (pModule.ModuleType = omtForm)
       and (Length(pModule.FileName) <> 0)
       and AnsiSameText(ExtractFileExt(pModule.FileName), '.pas')
       and FileExists(pModule.FileName);
  end;

var
  I, k: Integer;
  Module: IOTAModule;
  OpenFlag: Boolean;
  Wait: IWaitForm;

begin
  // ������ ������� �������
  k := 0;
  for i := 0 to AProject.GetModuleCount - 1 do
    if PasFrmModule(AProject.GetModule(i)) then
      Inc(k);

  Wait := TWaitForm.Create(Self, k);

  for i := 0 to AProject.GetModuleCount - 1 do
    if PasFrmModule(AProject.GetModule(i)) then
      begin
        OpenFlag := TCzToolsAPI.ModuleIsOpen(AProject.GetModule(i));
        Module := AProject.GetModule(i).OpenModule;

        SpellCheckModule(Module);

        if not OpenFlag then
          Module.Close;

        Wait.IncProgress;
      end;
end;


procedure TFormCzComponentsSpellChecker.FindInProjectGroup;
var
  i: Integer;
begin
  for i := 0 to ModuleServices.MainProjectGroup.ProjectCount - 1 do
    FindInProject(ModuleServices.MainProjectGroup.Projects[i]);
end;

procedure TFormCzComponentsSpellChecker.FormDestroy(Sender: TObject);
begin
  mIgnoreList.Lines.SaveToFile(FIgnoreListFileName);

  FreeAndNil(FSpellChecker);


  FreeAndNil(FIgnoreComponentName);
  FreeAndNil(FIgnoreComponentType);
  FreeAndNil(FIgnoreProperty);
  FreeAndNil(FIgnoreCompProp);
end;

procedure TFormCzComponentsSpellChecker.SetDictionaryName(
  const ADictionaryName: string);
begin
  lbCurrentDictionary.Caption := 'Current dictionary: ' + ADictionaryName;
end;

procedure TFormCzComponentsSpellChecker.SpellCheckModule(AModule: IOTAModule);
var
  FormEditor: IOTAFormEditor;

  FormEditorShowFlag: Boolean;
  TempEditor: IOTAEditor;

  ModuleInfo: IOTAModuleInfo;


  procedure SpellCheckComponent(AComponent: IOTAComponent);
  var
    i: Integer;
    PropValue, CompName, PropName: string;
    Misspell: TMisspell;
    StringsComponent: IOTAComponent;
  begin
    AComponent.GetPropValueByName('name', CompName);

    if Length(CompName) = 0 then
      Exit;

    if FIgnoreComponentName.Contains(CompName) then
      Exit;

    for i := 0 to AComponent.GetPropCount - 1 do
    begin
      if not (AComponent.GetPropType(i) in
         [tkClass, tkString, tkLString, tkWString, tkUString])
      then
        Continue;

      PropName := AComponent.GetPropName(i);

      if FIgnoreComponentType.Contains(AComponent.GetComponentType) then
        Continue;

      if FIgnoreProperty.Contains(PropName) then
        Continue;

      case AComponent.GetPropType(i) of
        tkClass:
        begin
          AComponent.GetPropValue(i, StringsComponent);
          if Assigned(StringsComponent) and AnsiSameText(StringsComponent.GetComponentType, 'TStrings') then
            PropValue := TStrings(StringsComponent.GetComponentHandle).Text
          else
            Continue;
        end;

        tkString, tkLString, tkWString, tkUString:
        begin
          PropValue := TCzToolsAPI.GetStringProperty(AComponent, i);

          if AnsiLowerCase(PropName) = 'caption'  then
            PropValue := StringReplace(PropValue, '&', '', [rfReplaceAll]);

      //    if FSpellChecker.FindWordInCustomWordList(PropValue) then
      //      Continue;
        end;

      else
        Continue;
      end;

      if FSpellChecker.SpellString(PropValue) then
      begin
        for Misspell in FSpellChecker.MisspellList do
        begin
          FFormResult.AddResult(PropValue,
                                Copy(PropValue, Misspell.Pos, Misspell.Length),
                                ExtractFileName(TCzToolsAPI.GetProjectFromModule(FormEditor.Module).FileName),
                                PropName, CompName, AComponent.GetComponentType, ModuleInfo, Misspell.Suggestions);
        end;
      end;
    end;


    for i := 0 to AComponent.GetComponentCount - 1 do
      SpellCheckComponent(AComponent.GetComponent(i));
  end;
begin
  Assert(Assigned(AModule), 'TKoFormSpellChecker.SpellCheckModule: AModule = nil');

  ModuleInfo := TCzToolsAPI.GetModuleInfoFromModule(AModule);
  if not Assigned(ModuleInfo) then
    Exit;

  if not (ModuleInfo.ModuleType = omtForm) then
    Exit;

  FormEditor := TCzToolsAPI.GetFormEditor(AModule);
  if not Assigned(FormEditor) then
    Exit;

  FormEditorShowFlag := AModule.CurrentEditor = FormEditor;

  if not FormEditorShowFlag then
  begin
    TempEditor := AModule.CurrentEditor;
    FormEditor.Show;
  end;

//  FormIgnore.SetIgnoreFilter(ExtractFileName(TKoOTAPI.GetProjectFromModule(AModule).FileName),
//                             ModuleInfo.FormName);

  try
    SpellCheckComponent(FormEditor.GetRootComponent);
  finally
    if not FormEditorShowFlag then
      TempEditor.Show;
  end;
end;

procedure TFormCzComponentsSpellChecker.UpdateIgnore;
var
  Line, Str: string;
begin
  FIgnoreProperty.Clear;
  FIgnoreComponentType.Clear;

  for Line in mIgnoreList.Lines do
  begin
    Str := Trim(Line);
    if Length(Str) <> 0 then
    begin
      if Str[1] = '.' then
      begin
        if Length(Str) > 1 then
        begin
          Delete(Str, 1, 1);
          FIgnoreProperty.Add(Trim(Str));
        end;
      end
      else
      if Str[1] = '#' then
      begin
        if Length(Str) > 1 then
        begin
          Delete(Str, 1, 1);
          FIgnoreComponentName.Add(Trim(Str));
        end;
      end
      else
      begin
      //  if  then


        FIgnoreComponentType.Add(Str);

      //  FIgnoreCompProp.Add();
      end;
    end;
  end;

end;

end.
