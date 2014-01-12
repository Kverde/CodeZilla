unit uKoSpellChecker;

interface
uses
  Classes,
   CCR.Hunspell_Cz,
     SysUtils,
       Forms,
        Generics.Collections,
         Types,
          uKoSingleton,
           uKoOptions;

const
  cDictPath = 'Dict\';

  cNewWordFileName = 'NewWord.txt';
  cDictListFileName = 'DictionariesList.txt';
  cOptionsFileName = 'SpellCheckerOptions.ini';

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

  THunspellsList = TObjectList<THunspell>;

  TSpellChecker = class;

  TDictionaries = class
  strict private
    FHunspellsList: THunspellsList;
    FSpellChecker: TSpellChecker;

    procedure AddCustomWords(AHunspell: THunspell);

  public
    constructor Create(ASpellChecker: TSpellChecker);
    destructor Destroy; override;

    procedure LoadFromFile;
    procedure SaveToFile;

    function Add(const ADictFileName: string): Boolean;
    procedure Delete(const ADictFileName: string);

    property HunspellsList: THunspellsList read FHunspellsList;
  end;

  TSpellCheckerOptions = class(TFormedOptions)
  strict private
    FDigitAsLetter: Boolean;
    FSymbolAsLetter: string;
  public

    [TIni('Options', 'DigitAsLetter', 'True')]
    [TOptions(okCheckBox, 'Определять цифры как буквы')]
    property DigitAsLetter: Boolean read FDigitAsLetter write FDigitAsLetter;

    [TIni('Options', 'SymbolAsLetter', '-._/')]
    [TOptions(okLabeledEdit, 'Символы определяемые как буквы')]
    property SymbolAsLetter: string read FSymbolAsLetter write FSymbolAsLetter;
  end;

  TSpellChecker = class(TTypedSingleton<TSpellChecker>)
  strict private
    FDictionaries: TDictionaries;

    FCustomWordList: TStringList;
    FMisspellList: TMisspellList;

    FStartWord, FLengthWord: Integer;

    FOptions: TSpellCheckerOptions;

    procedure LoadCustomWords;
    procedure SaveCustomWords;

    function NextWord(const AStr: string; var APos: Integer; out AWord: string): Boolean;


    procedure CheckWord(const AWord: string);

  strict protected
    procedure CreateSingleton; override;
    procedure DestroySingleton; override;
  public

    function AddCustomWord(const ACustomWord: string): Boolean;
    procedure DeleteCustomWord(const ACustomWord: string);
    // TODO
    //procedure EditCustomWord(const AOldWord, ANewWord: string);
    procedure GetCustomWordsList(AStringList: TStrings);

    function SpellString(const AStr: string): Boolean;

    function FindWordInCustomWordList(const AStr: string): Boolean;

    property MisspellList: TMisspellList read FMisspellList;
    property Dictionaries: TDictionaries read FDictionaries;
    property Options: TSpellCheckerOptions read FOptions;
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

procedure TSpellChecker.DestroySingleton;
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

procedure TSpellChecker.CreateSingleton;
var
  DictPath: string;
begin
  inherited;

  DictPath := gnrProgramDataPath + cDictPath;

  FCustomWordList := TStringList.Create;
  FMisspellList :=  TMisspellList.Create(True);

  LoadCustomWords;

  FDictionaries := TDictionaries.Create(Self);
  FDictionaries.LoadFromFile;

  FOptions := TSpellCheckerOptions.Create(gnrProgramUserDataPath + cOptionsFileName);
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

{ TDictionaries }

function TDictionaries.Add(const ADictFileName: string): Boolean;
var
  Hunspell: THunspell;
  FullFileName: string;

begin
  for Hunspell in FHunspellsList do
    if AnsiSameText(ExtractFileName(Hunspell.DictionaryFileName), ADictFileName) then
      Exit(False);

  Hunspell := THunspell.Create;
  Hunspell.TryLoadLibrary(gnrProgramDataPath + cHunspellDLLFileName);

  FullFileName := gnrProgramDataPath + cDictPath + ADictFileName;
  Hunspell.LoadDictionary(FullFileName, ChangeFileExt(FullFileName, '.aff'));

  AddCustomWords(Hunspell);
  FHunspellsList.Add(Hunspell);

  SaveToFile;

  Exit(True);
end;

procedure TDictionaries.AddCustomWords(AHunspell: THunspell);
var
  CustomWord: string;
  CustomWords: TStringList;

begin
  Assert(Assigned(AHunspell), 'TDictionaries.AddCustomWords: AHunspell = nil');

  //TODO оптимизивроавть

  CustomWords := TStringList.Create;
  try
    FSpellChecker.GetCustomWordsList(CustomWords);
    for CustomWord in CustomWords do
      AHunspell.AddCustomWord(CustomWord);
  finally
    FreeAndNil(CustomWords);
  end;
end;

constructor TDictionaries.Create(ASpellChecker: TSpellChecker);
begin
  FHunspellsList := THunspellsList.Create(True);

  FSpellChecker := ASpellChecker;
end;

procedure TDictionaries.Delete(const ADictFileName: string);
var
  Hunspell: THunspell;
begin
  for Hunspell in FHunspellsList do
    if AnsiSameText(ExtractFileName(Hunspell.DictionaryFileName), ADictFileName) then
      FHunspellsList.Delete(FHunspellsList.IndexOf(Hunspell));

  SaveToFile;
end;

destructor TDictionaries.Destroy;
begin
  FreeAndNil(FHunspellsList);

  inherited;
end;

procedure TDictionaries.LoadFromFile;
var
  StringList: TStringList;
  Hunspell: THunspell;
  DictFileName, FuulDictFileName: string;
begin
  FHunspellsList.Clear;

  StringList := TStringList.Create;
  try
    if FileExists(gnrProgramUserDataPath + cDictListFileName) then
      StringList.LoadFromFile(gnrProgramUserDataPath + cDictListFileName)
    else
      Exit;

    for DictFileName in StringList do
    begin
      FuulDictFileName := gnrProgramDataPath + cDictPath + DictFileName;
      if not FileExists(FuulDictFileName) then
        Continue;

      Hunspell := THunspell.Create;
      Hunspell.TryLoadLibrary(gnrProgramDataPath + cHunspellDLLFileName);
      Hunspell.LoadDictionary(FuulDictFileName, ChangeFileExt(FuulDictFileName, '.aff'));

      AddCustomWords(Hunspell);
      FHunspellsList.Add(Hunspell);
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TDictionaries.SaveToFile;
var
  StringList: TStringList;
  Hunspell: THunspell;
begin
  StringList := TStringList.Create;
  try
    for Hunspell in FHunspellsList do
      StringList.Add(ExtractFileName(Hunspell.DictionaryFileName));

    StringList.SaveToFile(gnrProgramUserDataPath + cDictListFileName);
  finally
    FreeAndNil(StringList);
  end;
end;

end.
