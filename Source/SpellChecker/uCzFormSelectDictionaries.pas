unit uCzFormSelectDictionaries;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, uKoSpellChecker, CCR.Hunspell_Cz, uKoGeneral,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin;

type
  TCzFormSelectDictionaries = class(TForm)
    lbDict: TListBox;
    dOpen: TOpenDialog;
    ToolBar: TToolBar;
    tbAddDictionary: TToolButton;
    tbDelDictionary: TToolButton;
    tbClose: TToolButton;
    ImageList: TImageList;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbAddDictionaryClick(Sender: TObject);
    procedure tbDelDictionaryClick(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
  private
    FSpellChecker: TSpellChecker;
  public
  end;

implementation

{$R *.dfm}

procedure TCzFormSelectDictionaries.FormCreate(Sender: TObject);
var
  Hunspell: THunspell;
begin
  FSpellChecker := TSpellChecker.Create;

  for Hunspell in FSpellChecker.Dictionaries.HunspellsList do
    lbDict.Items.Add(ExtractFileName(Hunspell.DictionaryFileName));
end;

procedure TCzFormSelectDictionaries.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpellChecker);
end;

procedure TCzFormSelectDictionaries.tbAddDictionaryClick(Sender: TObject);
begin
  dOpen.InitialDir := gnrProgramDataPath + cDictPath;

  if dOpen.Execute then
  begin
    Screen.Cursor := crSQLWait;
    try
      if FSpellChecker.Dictionaries.Add(ExtractFileName(dOpen.FileName)) then
        lbDict.Items.Add(ExtractFileName(dOpen.FileName));
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCzFormSelectDictionaries.tbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCzFormSelectDictionaries.tbDelDictionaryClick(Sender: TObject);
begin
  Screen.Cursor := crSQLWait;
  try
    if lbDict.ItemIndex >= 0 then
    begin
      FSpellChecker.Dictionaries.Delete(lbDict.Items[lbDict.ItemIndex]);
      lbDict.DeleteSelected;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
