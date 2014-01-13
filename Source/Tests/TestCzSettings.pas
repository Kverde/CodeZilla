unit TestCzSettings;

interface

uses
  Forms, SysUtils, Math, TestFramework, uCzSettings;

type

  Test_CzSettings = class(TTestCase)
  strict private
  public
    FLogFileName: string;

    procedure AfterConstruction; override;

    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure Settings_SaveLoad;

    procedure Settings_Default;
  end;

  TSetOfByte = set of Byte;

  TMyEnum = (meFirst, meSecond, meThird, meFourth);

  TMySettings = class(TCzSettings)
  strict private
    FValueChar: Char;
    FValueAnsiChar: AnsiChar;

    FValueCharW: Char;
    FValueAnsiCharW: AnsiChar;
































  public
    [TCzIni('�')]
    property ValueChar: Char read FValueChar write FValueChar;
    [TCzIni('�')]
    property ValueAnsiChar: AnsiChar read FValueAnsiChar write FValueAnsiChar;
    [TCzIni('�')]
    property ValueWideChar: WideChar read FValueWideChar write FValueWideChar;

    [TCzIni('w')]
    property ValueCharW: Char read FValueCharW write FValueCharW;
    [TCzIni('w')]
    property ValueAnsiCharW: AnsiChar read FValueAnsiCharW write FValueAnsiCharW;
    [TCzIni('w')]
    property ValueWideCharW: WideChar read FValueWideCharW write FValueWideCharW;

    [TCzIni('Value_������')]
    property ValueString: string read FValueString write FValueString;
    [TCzIni('Value_������')]







































//    property ValueSetOfByte: TSetOfByte read FValueSetOfByte write FValueSetOfByte;


    property ValueBoolean: Boolean read FValueBoolean write FValueBoolean;
    [TCzIni('True')]
    property ValueByteBool: ByteBool read FValueByteBool write FValueByteBool;
    [TCzIni('True')]
    property ValueWordBool: WordBool read FValueWordBool write FValueWordBool;
    [TCzIni('True')]
    property ValueLongBool: LongBool read FValueLongBool write FValueLongBool;
  end;


implementation

procedure Test_CzSettings.SetUp;
begin

end;

procedure Test_CzSettings.TearDown;
begin

end;

procedure Test_CzSettings.AfterConstruction;
begin
  inherited;

  FLogFileName := ExtractFilePath(Application.ExeName) + 'Settings.ini';
end;

procedure Test_CzSettings.Settings_Default;
var
  MySettings: TMySettings;

  procedure IsDefaultSettings(const AStep: string);
  begin
    CheckEquals(MySettings.ValueChar, '�');
    CheckEquals(MySettings.ValueAnsiChar, '�');
    CheckEquals(MySettings.ValueWideChar, '�');

    CheckEquals(MySettings.ValueCharW, 'w');
    CheckEquals(MySettings.ValueAnsiCharW, AnsiChar('w'));
    CheckEquals(MySettings.ValueWideCharW, 'w');

    CheckEquals(MySettings.ValueString, 'Value_������');
    CheckEquals(MySettings.ValueAnsiString, AnsiString('Value_������'));
    CheckEquals(MySettings.ValueWideString, 'Value_������');

    CheckEquals(MySettings.ValueShortInt, -120);
    CheckEquals(MySettings.ValueSmallInt, -30000);
    CheckEquals(MySettings.ValueInteger, -3000000);
    CheckEquals(MySettings.ValueInt64, -30000000000000);

    CheckEquals(MySettings.ValueByte, 120);

    CheckEquals(MySettings.ValueCardinal, 3000000);
    CheckEquals(MySettings.ValueUInt64, 30000000000000);



    CheckTrue(SameValue(MySettings.ValueExtended, 555.987), 'ValueExtended');
    CheckTrue(SameValue(MySettings.ValueReal, 555.986), 'ValueReal');

    CheckTrue(MySettings.ValueEnum = meSecond);



    CheckEquals(MySettings.ValueWordBool, True);
    CheckEquals(MySettings.ValueLongBool, True);
  end;

begin
  MySettings := TMySettings.Create(FLogFileName);
  try
    IsDefaultSettings('1');

    MySettings.Default;
    MySettings.Default;
    IsDefaultSettings('2');

    // Clear;

    MySettings.ValueChar     := #0;
    MySettings.ValueAnsiChar := #0;
    MySettings.ValueWideChar := #0;

    MySettings.ValueCharW     := #0;
    MySettings.ValueAnsiCharW := #0;
    MySettings.ValueWideCharW := #0;

    MySettings.ValueString     := '';
    MySettings.ValueAnsiString := '';
    MySettings.ValueWideString := '';

    MySettings.ValueShortInt := 0;
    MySettings.ValueSmallInt := 0;
    MySettings.ValueInteger  := 0;
    MySettings.ValueInt64    := 0;

    MySettings.ValueByte     := 0;
    MySettings.ValueWord     := 0;
    MySettings.ValueCardinal := 0;
    MySettings.ValueUInt64   := 0;

    MySettings.ValueSingle   := 0;

    MySettings.ValueExtended := 0;
    MySettings.ValueReal     := 0;










    MySettings.Default;
    IsDefaultSettings('3');
  finally
    MySettings.Destroy;
  end;

  MySettings := TMySettings.Create(FLogFileName, gnsdaSetDefault);
  try
    IsDefaultSettings('4');
  finally
    MySettings.Destroy;
  end;
end;

procedure Test_CzSettings.Settings_SaveLoad;
var
  MySettings: TMySettings;
begin
  MySettings := TMySettings.Create(FLogFileName);
  try
    MySettings.ValueChar     := '�';
    MySettings.ValueAnsiChar := '�';
    MySettings.ValueWideChar := '�';

    MySettings.ValueCharW     := 'r';
    MySettings.ValueAnsiCharW := 'r';
    MySettings.ValueWideCharW := 'r';

    MySettings.ValueString     := 'Value_������_save';
    MySettings.ValueAnsiString := 'Value_������_save';
    MySettings.ValueWideString := 'Value_������_save';

    MySettings.ValueShortInt := -121;
    MySettings.ValueSmallInt := -30001;
    MySettings.ValueInteger  := -3000001;
    MySettings.ValueInt64    := -30000000000001;

    MySettings.ValueByte     := 121;
    MySettings.ValueWord     := 30001;
    MySettings.ValueCardinal := 3000001;
    MySettings.ValueUInt64   := 30000000000001;

    MySettings.ValueSingle   := 666.654;

    MySettings.ValueExtended := 666.654;
    MySettings.ValueReal     := 666.654;








    MySettings.Save;
  finally
    MySettings.Destroy;
  end;

  MySettings := TMySettings.Create(FLogFileName, gnsdaNone);
  try
    MySettings.Load;

    CheckEquals(MySettings.ValueChar, '�');
    CheckEquals(MySettings.ValueAnsiChar, '�');
    CheckEquals(MySettings.ValueWideChar, '�');

    CheckEquals(MySettings.ValueCharW, 'r');
    CheckEquals(MySettings.ValueAnsiCharW, AnsiChar('r'));
    CheckEquals(MySettings.ValueWideCharW, 'r');

    CheckEquals(MySettings.ValueString, 'Value_������_save');
    CheckEquals(MySettings.ValueAnsiString, AnsiString('Value_������_save'));
    CheckEquals(MySettings.ValueWideString, 'Value_������_save');

    CheckEquals(MySettings.ValueShortInt, -121);
    CheckEquals(MySettings.ValueSmallInt, -30001);
    CheckEquals(MySettings.ValueInteger, -3000001);
    CheckEquals(MySettings.ValueInt64, -30000000000001);

    CheckEquals(MySettings.ValueByte, 121);

    CheckEquals(MySettings.ValueCardinal, 3000001);
    CheckEquals(MySettings.ValueUInt64, 30000000000001);



    CheckTrue(SameValue(MySettings.ValueExtended, 666.654), 'ValueExtended');
    CheckTrue(SameValue(MySettings.ValueReal, 666.654), 'ValueReal');





    CheckEquals(MySettings.ValueWordBool, False);
    CheckEquals(MySettings.ValueLongBool, False);
  finally
    MySettings.Destroy;
  end;
end;

initialization
  RegisterTest(Test_CzSettings.Suite);
end.
