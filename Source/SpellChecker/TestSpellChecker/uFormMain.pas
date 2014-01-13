unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uCzSpellChecker, ComCtrls, ExtCtrls, Menus;

type
  TFormMain = class(TForm)
    bSpellCheck: TButton;
    bDict: TButton;
    pSuggestions: TPanel;
    lbSuggestions: TListBox;
    Panel2: TPanel;
    pMisspell: TPanel;
    Panel3: TPanel;
    lbMisspell: TListBox;
    pText: TPanel;
    mText: TMemo;
    pMenu: TPanel;
    Label1: TLabel;
    pmMisspell: TPopupMenu;
    N1: TMenuItem;
    procedure bSpellCheckClick(Sender: TObject);
    procedure bDictClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbMisspellClick(Sender: TObject);
    procedure mTextChange(Sender: TObject);
    procedure lbSuggestionsDblClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FSpellChecker: TSpellChecker;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  uCzFormDictionaryEdit, uKoGeneral;


procedure TFormMain.bDictClick(Sender: TObject);
begin
  TCzFormDictionaryEdit.CreateAndShowModal;
end;

procedure TFormMain.bSpellCheckClick(Sender: TObject);
var
  Misspell: TMisspell;
begin
  lbMisspell.Clear;
  lbSuggestions.Clear;
  Screen.Cursor := crSQLWait;
  try
    bSpellCheck.Enabled := False;
    if FSpellChecker.SpellString(mText.Text) then
    begin
      for Misspell in FSpellChecker.MisspellList do
      begin
        lbMisspell.AddItem(Copy(mText.Text, Misspell.Pos, Misspell.Length), Misspell);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    bSpellCheck.Enabled := True;
  end;

end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  gnrProgramPath := ExtractFilePath(Application.ExeName);
  gnrProgramDataPath := gnrProgramPath + 'Data\';

  FSpellChecker := TSpellChecker.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpellChecker);
end;

procedure TFormMain.lbMisspellClick(Sender: TObject);
var
  Str: string;
  Misspell: TMisspell;
begin
  lbSuggestions.Clear;
  if lbMisspell.ItemIndex > -1 then
  begin
    Misspell := lbMisspell.Items.Objects[lbMisspell.ItemIndex] as TMisspell;
    for Str in Misspell.Suggestions do
      lbSuggestions.Items.Add(Str);

    mText.SelStart  := Misspell.Pos - 1;
    mText.SelLength := Misspell.Length;
    mText.SetFocus;
  end;
end;

procedure TFormMain.lbSuggestionsDblClick(Sender: TObject);
begin
  if lbSuggestions.ItemIndex > -1 then
  begin
    mText.SelText := lbSuggestions.Items[lbSuggestions.ItemIndex];
    lbMisspell.DeleteSelected;
    lbSuggestions.Clear;

    bSpellCheck.Click;
    mText.SetFocus;
  end;
end;

procedure TFormMain.mTextChange(Sender: TObject);
begin
  lbMisspell.Clear;
  lbSuggestions.Clear;
end;

procedure TFormMain.N1Click(Sender: TObject);
begin
  if lbMisspell.ItemIndex > -1 then
  begin
    FSpellChecker.AddCustomWord(lbMisspell.Items[lbMisspell.ItemIndex]);
    lbMisspell.DeleteSelected;
    lbSuggestions.Clear;

    mText.SetFocus;
  end;
end;

end.
