unit TesCzRtti;

interface

uses
  SysUtils, TypInfo, TestFramework, uGnRtti, Rtti, Math;

type

  Test_CzRtti = class(TTestCase)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GnRttiObject_GetAttribute;

    procedure GnValue_FromToString;

    procedure GnRtti_GetEnumName;
  end;

implementation

type
  TTestAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    constructor Create(const AName : string);
    property Name: string read FName;
  end;

  TTest2Attribute = class(TCustomAttribute)
  strict private
  public
  end;

  TMyEnum = (meFirst, meSecond, meThird, meFourth);

  TShortString = string[20];

  TSetOfByte = set of Byte;

  TMyClass = class
  strict private
    FField1: string;
    FField2: string;

    FValueChar: Char;
    FValueAnsiChar: AnsiChar;
    FValueWideChar: WideChar;

    FValueString: string;
    FValueAnsiString: AnsiString;
    FValueShortString: TShortString;
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

    FValueSetOfAnsiChar: TSysCharSet;
    FValueSetOfByte: TSetOfByte;

    FValueBoolean: Boolean;
    FValueByteBool: ByteBool;
    FValueWordBool: WordBool;
    FValueLongBool: LongBool;
  public
    [TTest('FirstProperty')]
    property Field1: string read FField1;

    [TTest('SecondProperty')]
    [TTest2]
    property Field2: string read FField2;

    property ValueChar: Char read FValueChar write FValueChar;
    property ValueAnsiChar: AnsiChar read FValueAnsiChar write FValueAnsiChar;
    property ValueWideChar: WideChar read FValueWideChar write FValueWideChar;

    property ValueString: string read FValueString write FValueString;
    property ValueAnsiString: AnsiString read FValueAnsiString write FValueAnsiString;
    property ValueShortString: TShortString read FValueShortString write FValueShortString;
    property ValueWideString: WideString read FValueWideString write FValueWideString;

    property ValueShortInt: ShortInt read FValueShortInt write FValueShortInt;
    property ValueSmallInt: SmallInt read FValueSmallInt write FValueSmallInt;
    property ValueInteger: Integer read FValueInteger write FValueInteger;
    property ValueInt64: Int64 read FValueInt64 write FValueInt64;

    property ValueByte: Byte read FValueByte write FValueByte;
    property ValueWord: Word read FValueWord write FValueWord;
    property ValueCardinal: Cardinal read FValueCardinal write FValueCardinal;
    property ValueUInt64: UInt64 read FValueUInt64 write FValueUInt64;

    property ValueSingle: Single read FValueSingle write FValueSingle;
    property ValueDouble: Double read FValueDouble write FValueDouble;
    property ValueExtended: Extended read FValueExtended write FValueExtended;
    property ValueReal: Real read FValueReal write FValueReal;

    property ValueEnum: TMyEnum read FValueEnum write FValueEnum;

    property ValueSetOfAnsiChar: TSysCharSet read FValueSetOfAnsiChar write FValueSetOfAnsiChar;
    property ValueSetOfByte: TSetOfByte read FValueSetOfByte write FValueSetOfByte;

    property ValueBoolean: Boolean read FValueBoolean write FValueBoolean;
    property ValueByteBool: ByteBool read FValueByteBool write FValueByteBool;
    property ValueWordBool: WordBool read FValueWordBool write FValueWordBool;
    property ValueLongBool: LongBool read FValueLongBool write FValueLongBool;
  end;


procedure Test_CzRtti.SetUp;
begin

end;

procedure Test_CzRtti.TearDown;
begin

end;

procedure Test_CzRtti.GnRtti_GetEnumName;
var
  MyEnum: TMyEnum;
begin
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), meFirst), 'meFirst');
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), meSecond), 'meSecond');
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), meThird), 'meThird');
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), meFourth), 'meFourth');

  MyEnum := meFirst;
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), MyEnum), 'meFirst');

  MyEnum := meSecond;
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), MyEnum), 'meSecond');

  MyEnum := meThird;
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), MyEnum), 'meThird');

  MyEnum := meFourth;
  CheckEquals(TGnRtti.GetEnumName(TypeInfo(TMyEnum), MyEnum), 'meFourth');
end;

procedure Test_CzRtti.GnValue_FromToString;
var
  RttiContext : TRttiContext;
  ObjType : TRttiType;
  Prop : TRttiProperty;

  MyObj: TMyClass;
  TempStr: string;

  procedure SaveValue(const APropertyName: string);
  var
    Value: TValue;
  begin
    Prop := ObjType.GetProperty(APropertyName);
    Value := Prop.GetValue(MyObj);
    TempStr := Value.GnToString;
  end;

  procedure LoadValue;
  var
    Value: TValue;
  begin
    Value := TValue.GnFromString(Prop.PropertyType.Handle, TempStr);
    Prop.SetValue(MyObj, Value);
  end;

begin
  RttiContext := TRttiContext.Create;
  ObjType := RttiContext.GetType(TMyClass.ClassInfo);

  MyObj := TMyClass.Create;
  try

    // Char

    MyObj.ValueChar := 'W';
    SaveValue('ValueChar');
    MyObj.ValueChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueChar, 'W');

    MyObj.ValueChar := 'ы';
    SaveValue('ValueChar');
    MyObj.ValueChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueChar, 'ы');

    MyObj.ValueAnsiChar := 'W';
    SaveValue('ValueAnsiChar');
    MyObj.ValueAnsiChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueAnsiChar, AnsiChar('W'));

    MyObj.ValueAnsiChar := 'ы';
    SaveValue('ValueAnsiChar');
    MyObj.ValueAnsiChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueAnsiChar, 'ы');

    MyObj.ValueWideChar := 'W';
    SaveValue('ValueWideChar');
    MyObj.ValueWideChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueWideChar, 'W');

    MyObj.ValueWideChar := 'ы';
    SaveValue('ValueWideChar');
    MyObj.ValueWideChar := #0;
    LoadValue;
    CheckEquals(MyObj.ValueWideChar, 'ы');

    // String

    MyObj.ValueString := 'MyString_йцукен';
    SaveValue('ValueString');
    MyObj.ValueString := '';
    LoadValue;
    CheckEquals(MyObj.ValueString, 'MyString_йцукен');

    MyObj.ValueAnsiString := 'MyAnsiString_йцукен';
    SaveValue('ValueAnsiString');
    MyObj.ValueAnsiString := '';
    LoadValue;
    CheckEquals(MyObj.ValueAnsiString, AnsiString('MyAnsiString_йцукен'));

//   Не работает
//    MyObj.ValueShortString := 'MyShortString';
//    SaveValue('ValueShortString');
//    MyObj.ValueShortString := '';
//    LoadValue;
//    CheckEquals(MyObj.ValueShortString, 'MyShortString');

    MyObj.ValueWideString := 'MyWideString_йцукен';
    SaveValue('ValueWideString');
    MyObj.ValueWideString := '';
    LoadValue;
    CheckEquals(MyObj.ValueWideString, 'MyWideString_йцукен');

    // Int

    MyObj.ValueShortInt := 120;
    SaveValue('ValueShortInt');
    MyObj.ValueShortInt := 0;
    LoadValue;
    CheckEquals(MyObj.ValueShortInt, 120);

    MyObj.ValueSmallInt := 30000;
    SaveValue('ValueSmallInt');
    MyObj.ValueSmallInt := 0;
    LoadValue;
    CheckEquals(MyObj.ValueSmallInt, 30000);

    MyObj.ValueInteger := 9999999;
    SaveValue('ValueInteger');
    MyObj.ValueInteger := 0;
    LoadValue;
    CheckEquals(MyObj.ValueInteger, 9999999);

    MyObj.ValueInt64 := -100000000000000;
    SaveValue('ValueInt64');
    MyObj.ValueInt64 := 0;
    LoadValue;
    CheckEquals(MyObj.ValueInt64, -100000000000000);

    MyObj.ValueByte := 200;
    SaveValue('ValueByte');
    MyObj.ValueByte := 0;
    LoadValue;
    CheckEquals(MyObj.ValueByte, 200);

    MyObj.ValueWord := 60000;
    SaveValue('ValueWord');
    MyObj.ValueWord := 0;
    LoadValue;
    CheckEquals(MyObj.ValueWord, 60000);

    MyObj.ValueCardinal := 66666666;
    SaveValue('ValueCardinal');
    MyObj.ValueCardinal := 0;
    LoadValue;
    CheckEquals(MyObj.ValueCardinal, 66666666);

    MyObj.ValueUInt64 := 100000000000000;
    SaveValue('ValueUInt64');
    MyObj.ValueUInt64 := 0;
    LoadValue;
    CheckEquals(MyObj.ValueUInt64, 100000000000000);

    // Float

    MyObj.ValueSingle := 123.123;
    SaveValue('ValueSingle');
    MyObj.ValueSingle := 0;
    LoadValue;
    CheckTrue(SameValue(MyObj.ValueSingle, 123.123, 0.00001), 'ValueSingle');

    MyObj.ValueDouble := 0.999;
    SaveValue('ValueDouble');
    MyObj.ValueDouble := 0;
    LoadValue;
    CheckTrue(SameValue(MyObj.ValueDouble, 0.999), 'ValueDouble');

    MyObj.ValueExtended := 223344.223344;
    SaveValue('ValueExtended');
    MyObj.ValueExtended := 0;
    LoadValue;
    CheckTrue(SameValue(MyObj.ValueExtended, 223344.223344), 'ValueExtended');

    MyObj.ValueReal := 0.999;
    SaveValue('ValueReal');
    MyObj.ValueReal := 0;
    LoadValue;
    CheckTrue(SameValue(MyObj.ValueReal, 0.999), 'ValueReal');

    // Enum

    MyObj.ValueEnum := meSecond;
    SaveValue('ValueEnum');
    MyObj.ValueEnum := meFirst;
    LoadValue;
    CheckTrue(MyObj.ValueEnum = meSecond);

    // Set - не работат

//    MyObj.ValueSetOfAnsiChar := ['a', 'b', 'c'];
//    MyObj.ValueSetOfAnsiChar := ['a'];
//    SaveValue('ValueSetOfAnsiChar');
//    MyObj.ValueSetOfAnsiChar := [];
//    LoadValue;
//    CheckTrue(MyObj.ValueSetOfAnsiChar = ['a', 'b', 'c']);

//    MyObj.ValueSetOfByte := [1..255];
//    SaveValue('ValueSetOfByte');
//    MyObj.ValueSetOfByte := [];
//    LoadValue;
//    CheckTrue(MyObj.ValueSetOfByte = [5, 135, 200]);

    // Boolean

    MyObj.ValueBoolean := True;
    SaveValue('ValueBoolean');
    MyObj.ValueBoolean := False;
    LoadValue;
    CheckTrue(MyObj.ValueBoolean);

    MyObj.ValueByteBool := True;
    SaveValue('ValueByteBool');
    MyObj.ValueByteBool := False;
    LoadValue;
    CheckTrue(MyObj.ValueByteBool);

    MyObj.ValueWordBool := True;
    SaveValue('ValueWordBool');
    MyObj.ValueWordBool := False;
    LoadValue;
    CheckTrue(MyObj.ValueWordBool);

    MyObj.ValueLongBool := True;
    SaveValue('ValueLongBool');
    MyObj.ValueLongBool := False;
    LoadValue;
    CheckTrue(MyObj.ValueLongBool);
  finally
    MyObj.Destroy;
  end;
end;

procedure Test_CzRtti.GnRttiObject_GetAttribute;
var
  RttiContext : TRttiContext;
  ObjType : TRttiType;
  Prop : TRttiProperty;

  TestAttribute : TTestAttribute;
  Test2Attribute : TTest2Attribute;

  i: Integer;
begin
  RttiContext := TRttiContext.Create;

  ObjType := RttiContext.GetType(TMyClass.ClassInfo);

  i := 0;

  for Prop in ObjType.GetProperties do
  begin
    TestAttribute := Prop.GetAttribute<TTestAttribute>;
    Test2Attribute := Prop.GetAttribute<TTest2Attribute>;

    case i of
      0:
      begin
        CheckNotNull(TestAttribute);
        CheckNull(Test2Attribute);

        CheckEquals(TestAttribute.Name, 'FirstProperty');
      end;

      1:
      begin
        CheckNotNull(TestAttribute);
        CheckNotNull(Test2Attribute);

        CheckEquals(TestAttribute.Name, 'SecondProperty');
      end;

      else
      begin
        CheckNull(TestAttribute);
        CheckNull(Test2Attribute);
      end;
    end;

    Inc(i);
  end;
end;

{ TTestAttribute }

constructor TTestAttribute.Create(const AName: string);
begin
  FName := AName;
end;

initialization

  RegisterTest(Test_CzRtti.Suite);
end.
