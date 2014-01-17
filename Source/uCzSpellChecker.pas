unit uCzSpellChecker;

interface
uses
  Classes,
  SysUtils,
  Generics.Collections,
  Types,
  Dialogs,
  Character,
  ShellAPI,

  uCzSettings,
  CCR.Hunspell_Cz;

const
  cDictFolder = 'Dict\';

  cCustomWordsFileName = 'CustomWords.txt';
  cSettingsFileName = 'SpellCheckerSettings.ini';

  cHunspellDLLFileName = 'hunspell.dll';

  cDefaultDictionary = 'ru_RU_2013.dic';

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
    FDigitAsLetter      : Boolean;
    FSymbolAsLetter     : string;
    FDictionaryFileName : string;
  public

    [TCzIni('True')]
    property DigitAsLetter: Boolean read FDigitAsLetter write FDigitAsLetter;

    [TCzIni('-._/')]
    property SymbolAsLetter: string read FSymbolAsLetter write FSymbolAsLetter;

    [TCzIni('')]
    property DictionaryFileName: string read FDictionaryFileName write FDictionaryFileName;

  end;

  TSpellChecker = class(TObject)
  strict private
    FDictPath     : string;
    FExpertPath   : string;

    FSettingFileName : string;


    FHunspell : THunspell;
    FSettings : TSpellCheckerSettings;

    FCustomWordList : TStringList;   // TODO hash
    FMisspellList   : TMisspellList;

    FStartWord, FLengthWord: Integer;

    procedure SaveCustomWords;
    procedure LoadCustomWords;
    procedure AddCusctomWordsInHunspell;
    procedure DelCustomWordsFromHunspell;

    procedure LoadDictionary(const ADictionaryFileName: string);

    function NextWord(const AStr: string; var APos: Integer; out AWord: string): Boolean;
    procedure CheckWord(const AWord: string);

    function GetDictionaryName: string;

  public
    constructor Create(const AExpertPath, ASettingsPath: string);
    destructor Destroy; override;

    function AddCustomWord(const ACustomWord: string): Boolean;
    procedure DeleteCustomWord(const ACustomWord: string);

    procedure ChoiceDictionary;
    procedure ShowCustomWordFile;
    procedure RefreshCustomWord;

    function SpellString(const AStr: string): Boolean;

    property MisspellList: TMisspellList read FMisspellList;
    property DictionaryName: string read GetDictionaryName;
  end;


implementation

function TSpellChecker.AddCustomWord(const ACustomWord: string): Boolean;
begin
  Result := ACustomWord.IndexOf(ACustomWord) > -1;

  // Если такое слово уже есть то выходим
  if not Result then
    Exit;

  FCustomWordList.Add(ACustomWord);
  FHunspell.AddCustomWord(ACustomWord);
  SaveCustomWords;

  Result := True;
end;

procedure TSpellChecker.CheckWord(const AWord: string);
var
  Misspell: TMisspell;
begin
  if FHunspell.IsSpeltCorrectly(AWord) then
    Exit;

  Misspell := TMisspell.Create(FStartWord, FLengthWord);
  Misspell.AddSuggestions(FHunspell.GetSuggestions(AWord));
  FMisspellList.Add(Misspell);
end;

procedure TSpellChecker.ChoiceDictionary;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    if Length(FSettings.DictionaryFileName) = 0 then
      OpenDialog.InitialDir := FDictPath
    else
      OpenDialog.InitialDir := ExtractFileDir(FSettings.DictionaryFileName);

    OpenDialog.Filter := 'Hunspell dictionary (*.dic)|*.dic';

    if OpenDialog.Execute then
      LoadDictionary(OpenDialog.FileName);

  finally
    FreeAndNil(OpenDialog);
  end;
end;

procedure TSpellChecker.DelCustomWordsFromHunspell;
var
  CustomWord: string;
begin
  for CustomWord in FCustomWordList do
    FHunspell.RemoveCustomWord(CustomWord);
end;

procedure TSpellChecker.DeleteCustomWord(const ACustomWord: string);
var
  Index: Integer;
begin
  Index := FCustomWordList.IndexOf(ACustomWord);
  if Index > 0 then
  begin
    FCustomWordList.Delete(Index);
    FHunspell.RemoveCustomWord(ACustomWord);

    SaveCustomWords;
  end;
end;

destructor TSpellChecker.Destroy;
begin
  FreeAndNil(FMisspellList);
  FreeAndNil(FCustomWordList);

  FreeAndNil(FHunspell);

  FreeAndNil(FSettings);

  inherited;
end;

function TSpellChecker.GetDictionaryName: string;
begin
  Result := ExtractFileName(FSettings.DictionaryFileName);
end;

constructor TSpellChecker.Create(const AExpertPath, ASettingsPath: string);
begin
  FExpertPath   := AExpertPath;
  FDictPath     := AExpertPath + cDictFolder;
  FSettingFileName := ASettingsPath + cSettingsFileName;

  FSettings       := TSpellCheckerSettings.Create(FSettingFileName);
  FMisspellList   := TMisspellList.Create(True);
  FCustomWordList := TStringList.Create;

  FHunspell := THunspell.Create;
  FHunspell.TryLoadLibrary(FExpertPath + cHunspellDLLFileName);

  LoadCustomWords;

  if Length(FSettings.DictionaryFileName) = 0 then
    FSettings.DictionaryFileName := FDictPath + cDefaultDictionary;

  LoadDictionary(FSettings.DictionaryFileName);
end;

procedure TSpellChecker.AddCusctomWordsInHunspell;
var
  CustomWord: string;
begin
  for CustomWord in FCustomWordList do
    FHunspell.AddCustomWord(CustomWord);
end;

procedure TSpellChecker.LoadCustomWords;
begin
  FCustomWordList.Clear;
  if FileExists(FSettingFileName) then
    FCustomWordList.LoadFromFile(FSettingFileName);
end;

procedure TSpellChecker.LoadDictionary(const ADictionaryFileName: string);
begin
  if not FileExists(ADictionaryFileName) then
  begin
    ShowMessage(Format('Dictionary %s not found', [FSettings.DictionaryFileName]));
    Exit;
  end;

  FHunspell.LoadDictionary(ADictionaryFileName,
                           ChangeFileExt(ADictionaryFileName, '.aff'));

  FSettings.DictionaryFileName := ADictionaryFileName;

  AddCusctomWordsInHunspell;
end;

function TSpellChecker.NextWord(const AStr: string; var APos: Integer; out AWord: string): Boolean;

  function IsAllowSymbol(const AChar: Char): boolean;
  begin
    Result := TCharacter.IsLetter(AChar)
              or (FSettings.DigitAsLetter and TCharacter.IsDigit(AChar))
              or (AnsiPos(AChar, FSettings.SymbolAsLetter) > 0);
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

procedure TSpellChecker.RefreshCustomWord;
begin
  DelCustomWordsFromHunspell;
  LoadCustomWords;
  AddCusctomWordsInHunspell;
end;

procedure TSpellChecker.SaveCustomWords;
begin
  FCustomWordList.SaveToFile(FSettingFileName);
end;

procedure TSpellChecker.ShowCustomWordFile;
begin
  ShellExecute(0, 'open', PChar(FSettingFileName), '', '', 0);
end;

function TSpellChecker.SpellString(const AStr: string): Boolean;
var
  CurPos: Integer;
  Word: string;

begin
  FMisspellList.Clear;
  CurPos := 1;

  if Length(AStr) = 0 then
    Exit(False);

  while NextWord(AStr, CurPos, Word) do
    CheckWord(Word);

  Result := FMisspellList.Count > 0;
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

end.
