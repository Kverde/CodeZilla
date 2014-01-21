unit uFormCzWait;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, ComCtrls;

type

  IWait = interface
  end;

  IWaitForm = interface(IWait)
    procedure IncProgress;
  end;

  TFormWait = class(TForm)
    ProgressBar: TProgressBar;
    Image1: TImage;
  end;

  TWait = class(TInterfacedObject, IWait)
  strict private
    FOwnerForm  : TForm;
    FTempCursor : Integer;
  public
    constructor Create(const AOwnerForm: TForm);
    destructor Destroy; override;
  end;

  TWaitForm = class(TWait, IWaitForm)
  strict private
    FFormWait: TFormWait;
  public
    constructor Create(const AOwnerForm: TForm; const AMax: Cardinal);

    procedure IncProgress;

    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TWait }

constructor TWait.Create(const AOwnerForm: TForm);
begin
  FOwnerForm := AOwnerForm;

  FOwnerForm.Enabled := False;

  FTempCursor   := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

destructor TWait.Destroy;
begin
  Application.ProcessMessages;

  Screen.Cursor := FTempCursor;
  FOwnerForm.Enabled := True;

  inherited;
end;


{ TWaitForm }

constructor TWaitForm.Create(const AOwnerForm: TForm; const AMax: Cardinal);
begin
  inherited Create(AOwnerForm);

  FFormWait := TFormWait.Create(AOwnerForm);
  FFormWait.ProgressBar.Max := AMax;
  FFormWait.Show;

  Application.ProcessMessages;
end;

destructor TWaitForm.Destroy;
begin
  FreeAndNil(FFormWait);
  inherited;
end;

procedure TWaitForm.IncProgress;
begin
  FFormWait.ProgressBar.Position := FFormWait.ProgressBar.Position + 1;

  Application.ProcessMessages;
end;

end.
