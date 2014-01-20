unit uFormCzResult;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, BaseDockForm, StdCtrls, ExtCtrls,
  Menus, Generics.Collections, ToolsAPI, Types, uCzToolsAPI,
  uCzSpellChecker;

type
  TResultRecord = record
    PopValue, Misspell,
    Project, PropertyName,
    ComponentName, ComponentType: string;
    ModuleInfo: IOTAModuleInfo;
    Suggestions: TStringDynArray;
  end;

  TResultList = TList<TResultRecord>;

  TFormCzResult = class(TBaseDockableForm)
    lbResult: TListBox;
    Panel1: TPanel;
    bClose: TButton;
    pmResult: TPopupMenu;
    miDeleteItem: TMenuItem;
    Addtodictionary1: TMenuItem;
    Addcomponenttypetoignorelist1: TMenuItem;
    Addpropertytoignorelist1: TMenuItem;
    bOpenSpellChecker: TButton;
    lbResultCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbResultDblClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure miDeleteItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Addtodictionary1Click(Sender: TObject);
    procedure Addcomponenttypetoignorelist1Click(Sender: TObject);
    procedure Addpropertytoignorelist1Click(Sender: TObject);
  private
    FResultList: TResultList;
    FSpellChecker: TSpellChecker;
    FIgnoreList: TStrings;
    FFormSpellChecker: TForm;

    procedure DeleteResult(const AIndex: Word);

  public
    constructor Create(AOwner: TComponent;
                       ASpellChecker: TSpellChecker;
                       AIgnoreList: TStrings;
                       AFormSpellChecker: TForm); reintroduce;

    procedure ClearResults;

    procedure RefreshCount;

    procedure AddResult(APopValue, AMisspell,
                        AProject, APropertyName,
                        AComponentName, AComponentType: string;
                        AModuleInfo: IOTAModuleInfo;
                        ASuggestions: TStringDynArray);


  end;

var
  FormCzResult: TFormCzResult;

implementation

{$R *.dfm}

{ TFormCzResult }

procedure TFormCzResult.Addcomponenttypetoignorelist1Click(Sender: TObject);
var
  CompType: string;
  i: Integer;
begin
  if lbResult.ItemIndex < 0 then
    Exit;

  CompType := FResultList[lbResult.ItemIndex].ComponentType;

  FIgnoreList.Add(CompType);

  DeleteResult(lbResult.ItemIndex);

  for i := FResultList.Count - 1 downto 0 do
    if AnsiSameText(CompType, FResultList[i].ComponentType) then
      DeleteResult(i);

  RefreshCount;
end;

procedure TFormCzResult.Addpropertytoignorelist1Click(Sender: TObject);
var
  PropName: string;
  i: Integer;
begin
  if lbResult.ItemIndex < 0 then
    Exit;

  PropName := FResultList[lbResult.ItemIndex].PropertyName;

  FIgnoreList.Add(PropName);

  DeleteResult(lbResult.ItemIndex);

  for i := FResultList.Count - 1 downto 0 do
    if AnsiSameText(PropName, FResultList[i].PropertyName) then
      DeleteResult(i);

  RefreshCount;
end;

procedure TFormCzResult.AddResult(APopValue, AMisspell, AProject, APropertyName,
  AComponentName, AComponentType: string; AModuleInfo: IOTAModuleInfo;
  ASuggestions: TStringDynArray);
var
  Res: TResultRecord;
begin
  Res.PopValue      := APopValue;
  Res.Misspell      := AMisspell;
  Res.Project       := AProject;
  Res.PropertyName  := APropertyName;
  Res.ComponentName := AComponentName;
  Res.ComponentType := AComponentType;

  Res.ModuleInfo  := AModuleInfo;
  Res.Suggestions := ASuggestions;

  FResultList.Add(Res);

  lbResult.Items.Add(AModuleInfo.FormName + '.' + AComponentName + '.' + APropertyName
    + ' - ' + AMisspell);
end;

procedure TFormCzResult.Addtodictionary1Click(Sender: TObject);
var
  DelWord: string;
  i: Integer;
begin
  inherited;

  if lbResult.ItemIndex < 0 then
    Exit;

  DelWord := FResultList[lbResult.ItemIndex].Misspell;

  FSpellChecker.AddCustomWord(DelWord);

  DeleteResult(lbResult.ItemIndex);

  for i := FResultList.Count - 1 downto 0 do
    if AnsiSameText(DelWord, FResultList[i].Misspell) then
      DeleteResult(i);

  RefreshCount;
end;


procedure TFormCzResult.bCloseClick(Sender: TObject);
begin
  inherited;

  Close;
  FFormSpellChecker.Show;
end;

procedure TFormCzResult.ClearResults;
begin
  FResultList.Clear;
  lbResult.Clear;
end;

constructor TFormCzResult.Create(AOwner: TComponent;
  ASpellChecker: TSpellChecker; AIgnoreList: TStrings;
  AFormSpellChecker: TForm);
begin
  inherited Create(AOwner);

  FSpellChecker := ASpellChecker;
  FIgnoreList   := AIgnoreList;

  FFormSpellChecker := AFormSpellChecker;
end;

procedure TFormCzResult.DeleteResult(const AIndex: Word);
begin
  FResultList.Delete(AIndex);
  lbResult.Items.Delete(AIndex);
end;

procedure TFormCzResult.FormCreate(Sender: TObject);
begin
  inherited;

  FResultList := TResultList.Create;
end;

procedure TFormCzResult.FormDestroy(Sender: TObject);
begin
  inherited;

  FreeAndNil(FResultList);
end;

procedure TFormCzResult.FormShow(Sender: TObject);
begin
  inherited;

  RefreshCount;
end;

procedure TFormCzResult.lbResultDblClick(Sender: TObject);
var
  FormEditor : IOTAFormEditor;
  Component  : IOTAComponent;

  Res: TResultRecord;
  FormName: string;

begin
  if FResultList.Count > 0 then
  begin
    Res := FResultList[lbResult.ItemIndex];

    FormEditor := TCzToolsAPI.GetFormEditor(Res.ModuleInfo.OpenModule);
    FormEditor.Show;

    Component := FormEditor.FindComponent(Res.ComponentName);

    if Assigned(Component) then
      Component.Select(False)
    else
    begin
      Component := FormEditor.GetRootComponent;
      if Assigned(Component) then
      begin
        Component.GetPropValueByName('Name', FormName);

        if AnsiSameText(FormName, Res.ComponentName) then
          Component.Select(False);
      end;
    end;
  end;
end;

procedure TFormCzResult.miDeleteItemClick(Sender: TObject);
begin
  inherited;

  if lbResult.ItemIndex > -1 then
  begin
    FResultList.Delete(lbResult.ItemIndex);
    lbResult.Items.Delete(lbResult.ItemIndex);

    RefreshCount;
  end;
end;

procedure TFormCzResult.RefreshCount;
begin
  lbResultCount.Caption := IntToStr(FResultList.Count);
end;

end.
