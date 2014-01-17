library CodeZillaDLL_XE4;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  BaseDockForm in '..\Source\BaseDockForm.pas' {BaseDockableForm},
  CCR.Hunspell_Cz in '..\Source\CCR.Hunspell_Cz.pas',
  uCzMain in '..\Source\uCzMain.pas',
  uCzRtti in '..\Source\uCzRtti.pas',
  uCzSettings in '..\Source\uCzSettings.pas',
  uCzSpellChecker in '..\Source\uCzSpellChecker.pas',
  uCzToolsAPI in '..\Source\uCzToolsAPI.pas',
  uFormCzComponentsSpellChecker in '..\Source\uFormCzComponentsSpellChecker.pas' {FormCzComponentsSpellChecker},
  uFormCzResult in '..\Source\uFormCzResult.pas' {FormCzResult};

{$R *.res}

exports
  InitWizard Name WizardEntryPoint;

begin
end.
