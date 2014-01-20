unit uCzGeneral;

interface

uses
  Forms, Controls;


type
  IWait = interface

  end;

  TWait = class(TInterfacedObject, IWait)
  strict private
    FForm: TForm;
    FTempCursor: Integer;
  public
    constructor Create(const AForm: TForm);
    destructor Destroy; override;
  end;

implementation


{ TWait }

constructor TWait.Create(const AForm: TForm);
begin
  FForm := AForm;

  FForm.Enabled := False;

  FTempCursor   := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

destructor TWait.Destroy;
begin
  Application.ProcessMessages;

  Screen.Cursor := FTempCursor;
  FForm.Enabled := True;

  inherited;
end;

end.
