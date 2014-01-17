library CodeZillaDLL_D2010;

uses
  EMemLeaks,
  EResLeaks,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EAppVCL,
  ExceptionLog7,
  SysUtils,
  Classes,
  ToolsAPI,
  BaseDockForm in '..\Source\BaseDockForm.pas' {BaseDockableForm},
  CCR.Hunspell_Cz in '..\Source\CCR.Hunspell_Cz.pas',
  uCzMain in '..\Source\uCzMain.pas',
  uCzRtti in '..\Source\uCzRtti.pas',
  uCzSettings in '..\Source\uCzSettings.pas',
  uCzSpellChecker in '..\Source\uCzSpellChecker.pas',
  uCzToolsAPI in '..\Source\uCzToolsAPI.pas',
  uCzComponentsSpellChecker in '..\Source\uCzComponentsSpellChecker.pas' {CzComponentsSpellChecker};

{$R *.res}

exports
  InitWizard Name WizardEntryPoint;

begin
end.
