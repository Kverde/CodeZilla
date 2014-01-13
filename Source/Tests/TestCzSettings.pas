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
    FValueWideChar: WideChar;
    FValueCharW: Char;
    FValueAnsiCharW: AnsiChar;
    FValueWideCharW: WideChar;

    FValueString: string;
    FValueAnsiString: AnsiString;
//    FValueShortString: TShortString;
    FValueWideString: WideString;

    FValueShortInt: ShortInt;
    FValueSmallInt: SmallInt;
    FValueInteger: Integer;
    FValueInt64: Int64;

    FValueByte: Byte;
    FValueWord: Word;
    FValueCardinal: Cardinal;
    FValueUInt64: UInt64;

    FValueSingle: Single;
    FValueDouble: Double;
    FValueExtended: Extended;
    FValueReal: Real;

    FValueEnum: TMyEnum;

//    FValueSetOfAnsiChar: TSysCharSet;
//    FValueSetOfByte: TSetOfByte;

    FValueBoolean: Boolean;
    FValueByteBool: ByteBool;
    FValueWordBool: WordBool;
    FValueLongBool: LongBool;

  public
    [TCzIni('ы')]
    property ValueChar: Char read FValueChar write FValueChar;
    [TCzIni('ы')]
    property ValueAnsiChar: AnsiChar read FValueAnsiChar write FValueAnsiChar;
    [TCzIni('ы')]
    property ValueWideChar: WideChar read FValueWideChar write FValueWideChar;

    [TCzIni('w')]
    property ValueCharW: Char read FValueCharW write FValueCharW;
    [TCzIni('w')]
    property ValueAnsiCharW: AnsiChar read FValueAnsiCharW write FValueAnsiCharW;
    [TCzIni('w')]
    property ValueWideCharW: WideChar read FValueWideCharW write FValueWideCharW;

    [TCzIni('Value_йцукен')]
    property ValueString: string read FValueString write FValueString;
    [TCzIni('Value_йцукен')]
    property ValueAnsiString: AnsiString read FValueAnsiString write FValueAnsiString;
//    [TCzIni('Value_йцукен')]
//    property ValueShortString: TShortString read FValueShortString write FValueShortString;
    [TCzIni('Value_йцукен')]
    property ValueWideString: WideString read FValueWideString write FValueWideString;

    [TCzIni('-120')]
    property ValueShortInt: ShortInt read FValueShortInt write FValueShortInt;
    [TCzIni('-30000')]
    property ValueSmallInt: SmallInt read FValueSmallInt write FValueSmallInt;
    [TCzIni('-3000000')]
    property ValueInteger: Integer read FValueInteger write FValueInteger;
    [TCzIni('-30000000000000')]
    property ValueInt64: Int64 read FValueInt64 write FValueInt64;

    [TCzIni('120')]
    property ValueByte: Byte read FValueByte write FValueByte;
    [TCzIni('30000')]
    property ValueWord: Word read FValueWord write FValueWord;
    [TCzIni('3000000')]
    property ValueCardinal: Cardinal read FValueCardinal write FValueCardinal;
    [TCzIni('30000000000000')]
    property ValueUInt64: UInt64 read FValueUInt64 write FValueUInt64;

    [TCzIni('555,98')]
    property ValueSingle: Single read FValueSingle write FValueSingle;
    [TCzIni('555,988')]
    property ValueDouble: Double read FValueDouble write FValueDouble;
    [TCzIni('555,987')]
    property ValueExtended: Extended read FValueExtended write FValueExtended;
    [TCzIni('555,986')]
    property ValueReal: Real read FValueReal write FValueReal;

    [TCzIni('meSecond')]
    property ValueEnum: TMyEnum read FValueEnum write FValueEnum;

//    [TCzIni('')]
//    property ValueSetOfAnsiChar: TSysCharSet read FValueSetOfAnsiChar write FValueSetOfAnsiChar;
//    [TCzIni('')]
//    property ValueSetOfByte: TSetOfByte read FValueSetOfByte write FValueSetOfByte;

    [TCzIni('True')]
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
    CheckEquals(MySettings.ValueChar, 'ы');
    CheckEquals(MySettings.ValueAnsiChar, 'ы');
    CheckEquals(MySettings.ValueWideChar, 'ы');

    CheckEquals(MySettings.ValueCharW, 'w');
    CheckEquals(MySettings.ValueAnsiCharW, AnsiChar('w'));
    CheckEquals(MySettings.ValueWideCharW, 'w');

    CheckEquals(MySettings.ValueString, 'Value_йцукен');
    CheckEquals(MySettings.ValueAnsiString, AnsiString('Value_йцукен'));
    CheckEquals(MySettings.ValueWideString, 'Value_йцукен');

    CheckEquals(MySettings.ValueShortInt, -120);
    CheckEquals(MySettings.ValueSmallInt, -30000);
    CheckEquals(MySettings.ValueInteger, -3000000);
    CheckEquals(MySettings.ValueInt64, -30000000000000);

    CheckEquals(MySettings.ValueByte, 120);
    CheckEquals(MySettings.ValueWord, 30000);
    CheckEquals(MySettings.ValueCardinal, 3000000);
    CheckEquals(MySettings.ValueUInt64, 30000000000000);

    CheckTrue(SameValue(MySettings.ValueSingle, 555.98, 0.01), 'ValueSingle');
    CheckTrue(SameValue(MySettings.ValueDouble, 555.988, 0.001), 'ValueDouble');
    CheckTrue(SameValue(MySettings.ValueExtended, 555.987), 'ValueExtended');
    CheckTrue(SameValue(MySettings.ValueReal, 555.986), 'ValueReal');

    CheckTrue(MySettings.ValueEnum = meSecond);

    CheckEquals(MySettings.ValueBoolean, True);
    CheckEquals(MySettings.ValueByteBool, True);
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
    MySettings.ValueDouble   := 0;
    MySettings.ValueExtended := 0;
    MySettings.ValueReal     := 0;

    MySettings.ValueEnum := meFirst;

    MySettings.ValueBoolean  := False;
    MySettings.ValueByteBool := False;
    MySettings.ValueWordBool := False;
    MySettings.ValueLongBool := False;

    // End clear

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
    MySettings.ValueChar     := 'й';
    MySettings.ValueAnsiChar := 'й';
    MySettings.ValueWideChar := 'й';

    MySettings.ValueCharW     := 'r';
    MySettings.ValueAnsiCharW := 'r';
    MySettings.ValueWideCharW := 'r';

    MySettings.ValueString     := 'Value_йцукен_save';
    MySettings.ValueAnsiString := 'Value_йцукен_save';
    MySettings.ValueWideString := 'Value_йцукен_save';

    MySettings.ValueShortInt := -121;
    MySettings.ValueSmallInt := -30001;
    MySettings.ValueInteger  := -3000001;
    MySettings.ValueInt64    := -30000000000001;

    MySettings.ValueByte     := 121;
    MySettings.ValueWord     := 30001;
    MySettings.ValueCardinal := 3000001;
    MySettings.ValueUInt64   := 30000000000001;

    MySettings.ValueSingle   := 666.654;
    MySettings.ValueDouble   := 666.654;
    MySettings.ValueExtended := 666.654;
    MySettings.ValueReal     := 666.654;

    MySettings.ValueEnum := meThird;

    MySettings.ValueBoolean  := False;
    MySettings.ValueByteBool := False;
    MySettings.ValueWordBool := False;
    MySettings.ValueLongBool := False;

    MySettings.Save;
  finally
    MySettings.Destroy;
  end;

  MySettings := TMySettings.Create(FLogFileName, gnsdaNone);
  try
    MySettings.Load;

    CheckEquals(MySettings.ValueChar, 'й');
    CheckEquals(MySettings.ValueAnsiChar, 'й');
    CheckEquals(MySettings.ValueWideChar, 'й');

    CheckEquals(MySettings.ValueCharW, 'r');
    CheckEquals(MySettings.ValueAnsiCharW, AnsiChar('r'));
    CheckEquals(MySettings.ValueWideCharW, 'r');

    CheckEquals(MySettings.ValueString, 'Value_йцукен_save');
    CheckEquals(MySettings.ValueAnsiString, AnsiString('Value_йцукен_save'));
    CheckEquals(MySettings.ValueWideString, 'Value_йцукен_save');

    CheckEquals(MySettings.ValueShortInt, -121);
    CheckEquals(MySettings.ValueSmallInt, -30001);
    CheckEquals(MySettings.ValueInteger, -3000001);
    CheckEquals(MySettings.ValueInt64, -30000000000001);

    CheckEquals(MySettings.ValueByte, 121);
    CheckEquals(MySettings.ValueWord, 30001);
    CheckEquals(MySettings.ValueCardinal, 3000001);
    CheckEquals(MySettings.ValueUInt64, 30000000000001);

    CheckTrue(SameValue(MySettings.ValueSingle, 666.654, 0.01), 'ValueSingle');
    CheckTrue(SameValue(MySettings.ValueDouble, 666.654, 0.001), 'ValueDouble');
    CheckTrue(SameValue(MySettings.ValueExtended, 666.654), 'ValueExtended');
    CheckTrue(SameValue(MySettings.ValueReal, 666.654), 'ValueReal');

    CheckTrue(MySettings.ValueEnum = meThird);

    CheckEquals(MySettings.ValueBoolean, False);
    CheckEquals(MySettings.ValueByteBool, False);
    CheckEquals(MySettings.ValueWordBool, False);
    CheckEquals(MySettings.ValueLongBool, False);
  finally
    MySettings.Destroy;
  end;
end;

initialization
  RegisterTest(Test_CzSettings.Suite);
end.

