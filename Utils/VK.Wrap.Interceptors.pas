unit VK.Wrap.Interceptors;

interface

uses
  Generics.Collections, System.SysUtils, TypInfo, System.Types, System.RTTI,
  Rest.Json, REST.JsonReflect, REST.Json.Interceptors, VK.Types;

type
  TEnumHelp = record
    type
      ETEnumHelpError = class(Exception);
    class function Cast<TEnum>(const Value: Integer): TEnum; static;
    class function Recast<TEnum>(const Value: TEnum): Integer; static;
  end;

  TJSONInterceptorStringToString = class(TJSONInterceptor)
  protected
    RTTI: TRttiContext;
  public
    constructor Create; reintroduce;
  end;

  TIntBooleanInterceptor = class(TJSONInterceptorStringToString)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TStringDateTimeInterceptor = class(TJSONInterceptorStringToString)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TEnumInterceptor<TEnum> = class(TJSONInterceptorStringToString)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TBirthDateVisibilityInterceptor = class(TEnumInterceptor<TVkBirthDateVisibility>)
    constructor Create; reintroduce;
  end;

  TPoliticalInterceptor = class(TEnumInterceptor<TVkPolitical>)
    constructor Create; reintroduce;
  end;

  TPlatformInterceptor = class(TEnumInterceptor<TVkPlatform>)
    constructor Create; reintroduce;
  end;

  TSexInterceptor = class(TEnumInterceptor<TVkSex>)
    constructor Create; reintroduce;
  end;

  TRelationInterceptor = class(TEnumInterceptor<TVkRelation>)
    constructor Create; reintroduce;
  end;

  TNameRequestStatusInterceptor = class(TEnumInterceptor<TVkNameRequestStatus>)
    constructor Create; reintroduce;
  end;

  TDocumentTypeInterceptor = class(TEnumInterceptor<TVkDocumentType>)
    constructor Create; reintroduce;
  end;

  TPeerTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TDeactivatedInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TAudioGenreInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TAttachmentTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TKeyboardButtonColorInterceptor = class(TEnumInterceptor<TVkKeyboardButtonColor>)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TMessageActionTypeInterceptor = class(TEnumInterceptor<TVkMessageActionType>)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

uses
  System.StrUtils;

{ TEnumHelp }

class function TEnumHelp.Cast<TEnum>(const Value: Integer): TEnum;
var
  typeInf: PTypeInfo;
  typeData: PTypeData;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
  if (typeInf = nil) or (typeInf^.Kind <> tkEnumeration) then
    raise ETEnumHelpError.Create('Not an enumeration type');
  typeData := GetTypeData(typeInf);
  if (Value < typeData^.MinValue) then
    raise ETEnumHelpError.CreateFmt('%d is below min value [%d]', [Value, typeData^.MinValue])
  else if (Value > typeData^.MaxValue) then
    raise ETEnumHelpError.CreateFmt('%d is above max value [%d]', [Value, typeData^.MaxValue]);
  case Sizeof(TEnum) of
    1:
      pByte(@Result)^ := Value;
    2:
      pWord(@Result)^ := Value;
    4:
      pCardinal(@Result)^ := Value;
  end;
end;

class function TEnumHelp.Recast<TEnum>(const Value: TEnum): Integer;
var
  typeInf: PTypeInfo;
  typeData: PTypeData;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
  if (typeInf = nil) or (typeInf^.Kind <> tkEnumeration) then
    raise ETEnumHelpError.Create('Not an enumeration type');
  typeData := GetTypeData(typeInf);
  case Sizeof(TEnum) of
    1:
      Result := pByte(@Value)^;
    2:
      Result := pWord(@Value)^;
    4:
      Result := pCardinal(@Value)^;
  end;
end;

{ TJSONInterceptorStringToString }

constructor TJSONInterceptorStringToString.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TEnumInterceptor<TEnum> }

constructor TEnumInterceptor<TEnum>.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TEnumInterceptor<TEnum>.StringConverter(Data: TObject; Field: string): string;
begin
  result := Ord(TEnumHelp.Recast<TEnum>(RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TEnum>)).ToString;
end;

procedure TEnumInterceptor<TEnum>.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<TEnum>(TEnumHelp.Cast<TEnum>(StrToIntDef(Arg, 0))));
end;

{ TStringDateTimeInterceptor }

constructor TStringDateTimeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TStringDateTimeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := DateToStr(RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>);
end;

procedure TStringDateTimeInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, StrToDateDef(Arg, 0));
end;

{ TDeactivatedInterceptor }

function TDeactivatedInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkDeactivated>.ToString;
end;

procedure TDeactivatedInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkDeactivated.Create(Arg)));
end;

{ TAudioGenreInterceptor }

function TAudioGenreInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkAudioGenre>.ToString;
end;

procedure TAudioGenreInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkAudioGenre.Create(StrToIntDef(Arg, 0))));
end;

{ TSexInterceptor }

constructor TSexInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TBirthDateVisibilityInterceptor }

constructor TBirthDateVisibilityInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TPoliticalInterceptor }

constructor TPoliticalInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TPlatformInterceptor }

constructor TPlatformInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TRelationInterceptor }

constructor TRelationInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TNameRequestStatusInterceptor }

constructor TNameRequestStatusInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TIntBooleanInterceptor }

constructor TIntBooleanInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TIntBooleanInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := IfThen(RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<Boolean>, '1', '0');
end;

procedure TIntBooleanInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, Arg = '1');
end;

{ TPeerTypeInterceptor }

function TPeerTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkPeerType>.ToString;
end;

procedure TPeerTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkPeerType.Create(Arg)));
end;

{ TAttachmentTypeInterceptor }

function TAttachmentTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkAttachmentType>.ToString;
end;

procedure TAttachmentTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkAttachmentType.Create(Arg)));
end;

{ TDocumentTypeInterceptor }

constructor TDocumentTypeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TKeyboardButtonColorInterceptor }

function TKeyboardButtonColorInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkKeyboardButtonColor>.ToString;
end;

procedure TKeyboardButtonColorInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkKeyboardButtonColor.Create(Arg)));
end;

{ TMessageActionTypeInterceptor }

function TMessageActionTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkMessageActionType>.ToString;
end;

procedure TMessageActionTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkMessageActionType.Create(Arg)));
end;

end.

