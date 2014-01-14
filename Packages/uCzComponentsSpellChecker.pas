unit uCzComponentsSpellChecker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TCzComponentsSpellChecker = class(TForm)
    Panel3: TPanel;
    rgKindFind: TRadioGroup;
    btnStart: TButton;
    bDictionary: TButton;
    Button1: TButton;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CzComponentsSpellChecker: TCzComponentsSpellChecker;

implementation

{$R *.dfm}

end.
