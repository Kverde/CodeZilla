program DemoSpellChecker;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  CCR.Hunspell_Cz in '..\CCR.Hunspell_Cz.pas',
  uKoSpellChecker in '..\uKoSpellChecker.pas',
  uKoFormDictionaryEdit in '..\uKoFormDictionaryEdit.pas' {KoFormDictionaryEdit},
  uCzFormSelectDictionaries in '..\uCzFormSelectDictionaries.pas' {CzFormSelectDictionaries},
  uKoGeneral in '..\..\KoGeneral\uKoGeneral.pas',
  uKoSingleton in '..\..\KoGeneral\uKoSingleton.pas',
  uKoFormOptions in '..\..\KoGeneral\KoOptions\uKoFormOptions.pas' {KoFormOptions},
  uKoOptions in '..\..\KoGeneral\KoOptions\uKoOptions.pas',
  uKoRtti in '..\..\KoGeneral\uKoRtti.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TKoFormOptions, KoFormOptions);
  Application.Run;
end.
