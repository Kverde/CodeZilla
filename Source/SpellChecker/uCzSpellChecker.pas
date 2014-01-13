unit uCzSpellChecker;

interface
uses
  Classes,
  CCR.Hunspell_Cz,
  SysUtils,
  Generics.Collections,
  Types,
  uCzSettings;

const
  cDictFolder = 'Dict\';

  cCustomWordsFileName = 'CustomWords.txt';
  cSettingsFileName = 'SpellCheckerSettings.ini';

  cHunspellDLLFileName = 'hunspell.dll';

type
  TMisspell = class
  strict private
    FPos, FLength: Integer;
    FSuggestions: TStringDynArray;

  public
    constructor Create(const APos, ALength: LongWord);
    destructor Destroy; override;

    procedure AddSuggestions(ASuggestions: TStringDynArray);

    property Pos    : Integer read FPos;
    property Length : Integer read FLength;
    property Suggestions: TStringDynArray read FSuggestions;
  end;

  TMisspellList = TObjectList<TMisspell>;

  TSpellCheckerSettings = class(TCzSettings)
  strict private
    FDigitAsLetter: Boolean;
    FSymbolAsLetter: string;
  public

    [TCzIni('True')]
    property DigitAsLetter: Boolean read FDigitAsLetter write FDigitAsLetter;

    [TCzIni('-._/')]
    property SymbolAsLetter: string read FSymbolAsLetter write FSymbolAsLetter;
  end;

  TSpellChecker = class(TObject)
  strict private
    FDictPath, FSettingsPath: string;

    FHunspell: THunspell;

    FCustomWordList: TStringList;
    FMisspellList: TMisspellList;

    FStartWord, FLengthWord: Integer;

    FSettings: TSpellCheckerSettings;

    procedure LoadCustomWords;
    procedure SaveCustomWords;

    function NextWord(const AStr: string; var APos: Integer; out AWord: string): Boolean;


    procedure CheckWord(const AWord: string);

  public
    constructor Create(const AExpertPath, ASettingsPath: string);
    destructor Destroy; override;


    function AddCustomWord(const ACustomWord: string): Boolean;
    procedure DeleteCustomWord(const ACustomWord: string);
    // TODO
    //procedure EditCustomWord(const AOldWord, ANewWord: string);
    procedure GetCustomWordsList(AStringList: TStrings);

    function SpellString(const AStr: string): Boolean;

    function FindWordInCustomWordList(const AStr: string): Boolean;

    property MisspellList: TMisspellList read FMisspellList;
  end;


implementation

uses
  Character, StrUtils, uKoGeneral;

function TSpellChecker.AddCustomWord(const ACustomWord: string): Boolean;
var
  Hunspell: THunspell;
begin
  Result := not FindWordInCustomWordList(ACustomWord);

  // Если такое слово уже есть то выходим
  if not Result then
    Exit;

  FCustomWordList.Add(ACustomWord);

  for Hunspell in FDictionaries.HunspellsList do
    Hunspell.AddCustomWord(ACustomWord);

  SaveCustomWords;
end;

procedure TSpellChecker.CheckWord(const AWord: string);
var
  Hunspell: THunspell;
  Misspell: TMisspell;
begin
  if FDictionaries.HunspellsList.Count <= 0 then
    Exit;

  for Hunspell in FDictionaries.HunspellsList do
    if Hunspell.IsSpeltCorrectly(AWord) then
      Exit;

  Misspell := TMisspell.Create(FStartWord, FLengthWord);

  for Hunspell in FDictionaries.HunspellsList do
    Misspell.AddSuggestions(Hunspell.GetSuggestions(AWord));

  FMisspellList.Add(Misspell);
end;

procedure TSpellChecker.DeleteCustomWord(const ACustomWord: string);
var
  Index: Integer;
  Hunspell: THunspell;
begin
  Index := FCustomWordList.IndexOf(ACustomWord);
  if Index > 0 then
  begin
    FCustomWordList.Delete(Index);

    for Hunspell in FDictionaries.HunspellsList do
      Hunspell.RemoveCustomWord(ACustomWord);

    SaveCustomWords;
  end;
end;

destructor TSpellChecker.Destroy;
begin
  FreeAndNil(FDictionaries);

  FreeAndNil(FMisspellList);
  FreeAndNil(FCustomWordList);

  FreeAndNil(FOptions);

  inherited;
end;

procedure TSpellChecker.GetCustomWordsList(AStringList: TStrings);
begin
  Assert(Assigned(AStringList), 'TSpellChecker.GetCustomWordsList: AStringList = nil');

  AStringList.Assign(FCustomWordList);
end;

constructor TSpellChecker.Create(const AExpertPath, ASettingsPath: string);
begin
  inherited;

  FDictPath := AExpertPath + cDictFolder;
  FSettingsPath := ASettingsPath;

  FSettings := TSpellCheckerSettings.Create(FSettingsPath + cSettingsFileName);





  FCustomWordList := TStringList.Create;




  FMisspellList :=  TMisspellList.Create(True);

  LoadCustomWords;



      if not FileExists(FuulDictFileName) then
        Continue;

      Hunspell := THunspell.Create;
      Hunspell.TryLoadLibrary(gnrProgramDataPath + cHunspellDLLFileName);
      Hunspell.LoadDictionary(FuulDictFileName, ChangeFileExt(FuulDictFileName, '.aff'));

      AddCustomWords(Hunspell);
      FHunspellsList.Add(Hunspell);
end;

procedure TSpellChecker.LoadCustomWords;
begin
  FCustomWordList.Clear;

  if FileExists(gnrProgramUserDataPath + cNewWordFileName) then
    FCustomWordList.LoadFromFile(gnrProgramUserDataPath + cNewWordFileName);
end;

function TSpellChecker.NextWord(const AStr: string; var APos: Integer; out AWord: string): Boolean;

  function IsAllowSymbol(const AChar: Char): boolean;
  begin
    Result := TCharacter.IsLetter(AChar)
              or (Options.DigitAsLetter and TCharacter.IsDigit(AChar))
              or (AnsiPos(AChar, Options.SymbolAsLetter) > 0);
  end;

begin
  while (APos <= Length(AStr))
         and not IsAllowSymbol(AStr[APos])
  do
    inc(APos);

  FStartWord := APos;

  while (APos <= Length(AStr))
         and IsAllowSymbol(AStr[APos])
  do
    inc(APos);

  FLengthWord := APos - FStartWord;

  Result := FLengthWord > 0;

  if Result then
    AWord := Copy(AStr,  FStartWord, FLengthWord)
  else
    AWord := '';
end;

procedure TSpellChecker.SaveCustomWords;
begin
  FCustomWordList.SaveToFile(gnrProgramUserDataPath + cNewWordFileName);
end;

function TSpellChecker.SpellString(const AStr: string): Boolean;
var
  CurPos: Integer;
  Word: string;

begin
  FMisspellList.Clear;
  CurPos := 1;

  if IsEmptyStr(AStr) then
    Exit(False);

  while NextWord(AStr, CurPos, Word) do
    CheckWord(Word);

  Result := FMisspellList.Count > 0;
end;

function TSpellChecker.FindWordInCustomWordList(const AStr: string): Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to FCustomWordList.Count - 1 do
    if AStr = FCustomWordList[i] then
      Exit(True);
end;

{ TMisspell }

procedure TMisspell.AddSuggestions(ASuggestions: TStringDynArray);
var
  NewStr: string;
begin
  for NewStr in ASuggestions do
  begin
    SetLength(FSuggestions, System.Length(FSuggestions) + 1);
    FSuggestions[High(FSuggestions)] := NewStr;
  end;
end;

constructor TMisspell.Create(const APos, ALength: LongWord);
begin
  FPos    := APos;
  FLength := ALength;
end;

destructor TMisspell.Destroy;
begin
  SetLength(FSuggestions, 0);
  inherited;
end;

//procedure TDictionaries.AddCustomWords(AHunspell: THunspell);
//var
//  CustomWord: string;
//  CustomWords: TStringList;
//
//begin
//  Assert(Assigned(AHunspell), 'TDictionaries.AddCustomWords: AHunspell = nil');
//
//  //TODO оптимизивроавть
//
//  CustomWords := TStringList.Create;
//  try
//    FSpellChecker.GetCustomWordsList(CustomWords);
//    for CustomWord in CustomWords do
//      AHunspell.AddCustomWord(CustomWord);
//  finally
//    FreeAndNil(CustomWords);
//  end;
//end;


end.
