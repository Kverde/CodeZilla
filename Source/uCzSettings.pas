unit uCzSettings;

interface

uses
  Windows, Classes, Rtti, TypInfo, SysUtils, Forms, Generics.Collections,
  uCzRtti;

const
  cDefaultSectionName = 'Settings';

type
  ECzSettings = class(Exception);
  ECzSettings_TypeNotSupported = class(ECzSettings);

  TCzIniAttribute = class(TCustomAttribute)
  strict private
    FSection      : string;
    FName         : string;
    FDefaultValue : string;
  public
    constructor Create(const ASection, AName, ADefaultValue: string); overload;
    constructor Create(const AName, ADefaultValue : string); overload;
    constructor Create(const ADefaultValue : string); overload;

    property Section      : string read FSection write FSection;
    property Name         : string read FName write FName;
    property DefaultValue : string read FDefaultValue write FDefaultValue;
  end;

  TCzSettingsDefaultAction = (gnsdaNone, gnsdaLoad, gnsdaSetDefault);

  TCzSettings = class(TObject)
  strict private
    FFileName: string;
  public
    constructor Create(const AFileName: string;
      const AGnSettingsDefaultAction: TCzSettingsDefaultAction = gnsdaSetDefault); reintroduce;

    destructor Destroy; override;

    procedure Save; virtual;
    procedure Load; virtual;
    procedure Default; virtual;
  end;

implementation

uses
  IniFiles, StdCtrls, Controls, ExtCtrls;

{ TOptions }

constructor TCzSettings.Create(const AFileName: string;
  const AGnSettingsDefaultAction: TCzSettingsDefaultAction = gnsdaSetDefault);
begin
  inherited Create;

  FFileName := AFileName;

  case AGnSettingsDefaultAction of
    gnsdaLoad: Load;
    gnsdaSetDefault: Default;
  end;
end;

procedure TCzSettings.Default;
var
  RttiContext : TRttiContext;
  ObjType : TRttiType;

  Prop : TRttiProperty;
  Value : TValue;
  GnIniValue : TCzIniAttribute;
begin
  RttiContext := TRttiContext.Create;

  ObjType := RttiContext.GetType(ClassInfo);

  for Prop in ObjType.GetProperties do
  begin
    GnIniValue := Prop.GetAttribute<TCzIniAttribute>;
    if Assigned(GnIniValue) then
    begin
      Value := TValue.GnFromString(Prop.PropertyType.Handle, GnIniValue.DefaultValue);
      Prop.SetValue(Self, Value);
    end;
  end;
end;

destructor TCzSettings.Destroy;
begin

  inherited;
end;

procedure TCzSettings.Load;
var
  RttiContext : TRttiContext;
  ObjType : TRttiType;

  Prop : TRttiProperty;
  Value : TValue;
  IniValue : TCzIniAttribute;
  IniFile : TIniFile;
  Data : String;
begin
  RttiContext := TRttiContext.Create;

  IniFile := TIniFile.Create(FFileName);
  try
    ObjType := RttiContext.GetType(ClassInfo);

    for Prop in ObjType.GetProperties do
    begin
      IniValue := Prop.GetAttribute<TCzIniAttribute>;
      if Assigned(IniValue) then
      begin
        if Length(IniValue.Name) = 0 then
          IniValue.Name := Prop.Name;

        Data := IniFile.ReadString(IniValue.Section, IniValue.Name, IniValue.DefaultValue);
        Value := TValue.GnFromString(Prop.PropertyType.Handle, Data);
        Prop.SetValue(Self, Value);
      end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TCzSettings.Save;
var
  RttiContext : TRttiContext;
  ObjType : TRttiType;
  Prop : TRttiProperty;

  Value : TValue;
  IniValue : TCzIniAttribute;
  IniFile : TIniFile;
  Data : String;
begin
  RttiContext := TRttiContext.Create;
  DeleteFile(FFileName);

  IniFile := TIniFile.Create(FFileName);
  try
    ObjType := RttiContext.GetType(ClassInfo);
    for Prop in ObjType.GetProperties do
    begin
      IniValue := Prop.GetAttribute<TCzIniAttribute>;
      if Assigned(IniValue) then
      begin
        if Length(IniValue.Name) = 0 then
          IniValue.Name := Prop.Name;

        Value := Prop.GetValue(Self);
        Data := Value.GnToString;
        IniFile.WriteString(IniValue.Section, IniValue.Name, Data);
      end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

{ IniValueAttribute }

constructor TCzIniAttribute.Create(const ASection, AName, ADefaultValue: string);
begin
  FSection := ASection;
  FName    := AName;

  FDefaultValue := ADefaultValue;
end;

constructor TCzIniAttribute.Create(const AName, ADefaultValue: string);
begin
  FSection := cDefaultSectionName;
  FName    := AName;

  FDefaultValue := ADefaultValue;
end;

constructor TCzIniAttribute.Create(const ADefaultValue: string);
begin
  FSection := cDefaultSectionName;
  FName    := '';

  FDefaultValue := ADefaultValue;
end;

end.
