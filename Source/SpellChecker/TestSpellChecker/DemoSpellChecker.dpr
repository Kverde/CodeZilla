program DemoSpellChecker;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  CCR.Hunspell_Cz in '..\CCR.Hunspell_Cz.pas',
  uCzSpellChecker in '..\uCzSpellChecker.pas',
  uCzFormDictionaryEdit in '..\uCzFormDictionaryEdit.pas' {CzFormDictionaryEdit},
  uCzRtti in '..\..\uCzRtti.pas',
  uCzSettings in '..\..\uCzSettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
