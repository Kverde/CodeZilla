unit uCzRtti;

interface

uses
  Rtti, SysUtils, TypInfo, Variants;

type
  ECzRtti = class(Exception);

  TCzRtti = class
  public
    // Возвращает строковое представление элемента перечисляемого типа
    // пример использования
    // type TMyEnumType = (metFirst, metSecond);
    // var MyEnumValue: TMyEnumType;
    // ...
    // ShowMessage(TGnRtti.GetEnumName(System.TypeInfo(TMyEnumType), MyEnumValue))

    class function GetEnumName(const ATypeInfo: PTypeInfo; const AValue: Variant): string;
  end;

  TCzRttiObjectHelper = class helper for TRttiObject
  public
    // Возвращает первый атрибут заданного типа
    //  Если атрибут не найден возвращает nil
    function GetAttribute<T: class>: T;
  end;

  TCzValueHelper = record helper for TValue
  public
    // Преобразует Value в строку
    //  Если тип Value не поддреживается возбуждает исключение EGnRtti
    function GnToString: string;
    // Преобразует строку в Value
    //  Если тип Value не поддреживается возбуждает исключение EGnRtti
    class function GnFromString(const ATypeInfo: PTypeInfo; const AStrValue: string) : TValue; static;
  end;

implementation

{ TGnRttiObjectHelper }

function TCzRttiObjectHelper.GetAttribute<T>: T;
var
  Attr: TCustomAttribute;
begin
  for Attr in Self.GetAttributes do
  begin
    if Attr.ClassNameIs(T.ClassName) then
      Exit(T(Attr));
  end;
  Result := nil;
end;

{ TGnValueHelper }


class function TCzValueHelper.GnFromString(const ATypeInfo: PTypeInfo; const AStrValue: string) : TValue;
//var
//  I : Integer;
//  ShortString: string[50];

begin
{ TTypeKind:

  tkUnknown - Identifies an unknown type that has RTTI.

  tkChar - Identifies a single-byte character.
  tkWChar - Identifies a 2-byte (wide) character type.

  tkUString - Identifies a UnicodeString type.
  tkLString  -Identifies an AnsiString type.
  tkWString - Identifies a WideString type.
  tkString - Identifies a short string type.

  tkInteger - Identifies an ordinal type.
  tkInt64 - Identifies the Int64/UInt64 types.

  tkFloat - Identifies a floating-point type.

  tkEnumeration - Identifies an enumeration type.
  tkSet - Identifies a set type.
}

   case ATypeInfo.Kind of
    tkChar,
    tkWChar: Result := TValue.From<string>(AStrValue);

    tkLString : Result := TValue.From<AnsiString>(AnsiString(AStrValue));
    tkWString : Result := TValue.From<WideString>(WideString(AStrValue));
    tkUString : Result := TValue.From<string>(AStrValue);

//    tkString :  Не работает
//      begin
//        ShortString := AStrValue;
//        Self.Make(@AStrValue[1], Self.TypeInfo, Self);
//      end;

    tkInteger : Result := StrToInt(AStrValue);
    tkInt64   : Result := StrToInt64(AStrValue);
    tkFloat   : Result := StrToFloat(AStrValue);

    tkEnumeration: Result := TValue.FromOrdinal(ATypeInfo, GetEnumValue(ATypeInfo, AStrValue));
//    tkSet:
//      begin
//        i := StringToSet(Self.TypeInfo, AStrValue);
//        TValue.Make(@i, Self.TypeInfo, Self);
//      end;
  else
    raise ECzRtti.CreateFmt('Type %s not supported', [TCzRtti.GetEnumName(System.TypeInfo(TTypeKind), ATypeInfo.Kind)]);
  end;
end;

function TCzValueHelper.GnToString: string;
//var
//  i : Integer;
begin
  case Self.Kind of
    tkChar,
    tkWChar,

    tkLString,
    tkWString,
    tkUString,
//    tkString,

    tkInteger,
    tkInt64,

    tkFloat,

  //  tkSet,
    tkEnumeration:
      begin
        Result := Self.ToString;
      end;

    //tkSet не работает Self.ToString возвращает не полное множество, только елементы 0..7
    // Возможно где-то размер множетсва урезается до одного байта.

  else
    raise ECzRtti.CreateFmt('Type %s not supported', [TCzRtti.GetEnumName(System.TypeInfo(TTypeKind), Self.Kind)]);
  end;
end;

{ TGnRtti }

class function TCzRtti.GetEnumName(const ATypeInfo: PTypeInfo; const AValue: Variant): string;
begin
  Result := TypInfo.GetEnumName(ATypeInfo, AValue);
end;

end.
