unit uCzFormDictionaryEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, uCzSpellChecker;

type
  TCzFormDictionaryEdit = class(TForm)
    LBDopWordsForDic: TListBox;
    PanelButtons: TPanel;
    bAdd: TButton;
    eWord: TEdit;
    bEdit: TButton;
    bDelete: TButton;
    bClose: TButton;
    bSelectDict: TButton;
    bOptions: TButton;
    procedure FormShow(Sender: TObject);
    procedure LBDopWordsForDicClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bEditClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bSelectDictClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bOptionsClick(Sender: TObject);
  strict private
    FSpellChecker: TSpellChecker;

    procedure DeleteWord;
  public

  end;



implementation

{$R *.dfm}

uses
  uKoGeneral, uCzFormSelectDictionaries;

{ TFormDictionaryEdit }

procedure TCzFormDictionaryEdit.bAddClick(Sender: TObject);
begin
  if not IsEmptyStr(eWord.Text) and FSpellChecker.AddCustomWord(eWord.Text) then
    LBDopWordsForDic.ItemIndex := LBDopWordsForDic.Items.Add(eWord.Text);
end;

procedure TCzFormDictionaryEdit.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCzFormDictionaryEdit.bDeleteClick(Sender: TObject);
begin
  if LBDopWordsForDic.ItemIndex > -1 then
  begin
    FSpellChecker.DeleteCustomWord(LBDopWordsForDic.Items[LBDopWordsForDic.ItemIndex]);
    DeleteWord;
  end;
end;

procedure TCzFormDictionaryEdit.bEditClick(Sender: TObject);
begin
  if LBDopWordsForDic.ItemIndex > -1 then
    if IsEmptyStr(eWord.Text) then
    begin
      FSpellChecker.DeleteCustomWord(LBDopWordsForDic.Items[LBDopWordsForDic.ItemIndex]);
      DeleteWord;
    end
    else
    begin
      FSpellChecker.DeleteCustomWord(LBDopWordsForDic.Items[LBDopWordsForDic.ItemIndex]);
      LBDopWordsForDic.Items.Delete(LBDopWordsForDic.ItemIndex);

      if FSpellChecker.AddCustomWord(eWord.Text) then
        LBDopWordsForDic.ItemIndex := LBDopWordsForDic.Items.Add(eWord.Text);
    end;
end;

procedure TCzFormDictionaryEdit.bSelectDictClick(Sender: TObject);
begin
  TCzFormSelectDictionaries.CreateAndShowModal;
end;

procedure TCzFormDictionaryEdit.bOptionsClick(Sender: TObject);
begin
  FSpellChecker.Options.ShowModal;
end;

procedure TCzFormDictionaryEdit.DeleteWord;
var
  TemItemIndex: Integer;
begin
  TemItemIndex := LBDopWordsForDic.ItemIndex;
  LBDopWordsForDic.Items.Delete(LBDopWordsForDic.ItemIndex);

  if LBDopWordsForDic.Items.Count > -1 then
    if TemItemIndex > 0 then
      LBDopWordsForDic.ItemIndex := TemItemIndex - 1
    else
      LBDopWordsForDic.ItemIndex := TemItemIndex;
end;

procedure TCzFormDictionaryEdit.FormCreate(Sender: TObject);
begin
  FSpellChecker := TSpellChecker.Create;
end;

procedure TCzFormDictionaryEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpellChecker);
end;

procedure TCzFormDictionaryEdit.FormShow(Sender: TObject);
begin
  FSpellChecker.GetCustomWordsList(LBDopWordsForDic.Items);
end;

procedure TCzFormDictionaryEdit.LBDopWordsForDicClick(Sender: TObject);
begin
  if LBDopWordsForDic.ItemIndex > -1 then
    eWord.Text := LBDopWordsForDic.Items[LBDopWordsForDic.ItemIndex];
end;

end.
