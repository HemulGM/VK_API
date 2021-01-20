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

  TStringDateTimeInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TJSONInterceptorStringToString = class(TJSONInterceptor)
    constructor Create; reintroduce;
  end;

  TEnumInterceptor<TEnum> = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TBirthDateVisibilityInterceptor = TEnumInterceptor<TVkBirthDateVisibility>;

  TPoliticalInterceptor = TEnumInterceptor<TVkPolitical>;

  TPlatformInterceptor = TEnumInterceptor<TVkPlatform>;

  TSexInterceptor = TEnumInterceptor<TVkSex>;

  TRelationInterceptor = TEnumInterceptor<TVkRelation>;

  TNameRequestStatusInterceptor = TEnumInterceptor<TVkNameRequestStatus>;

  TDeactivatedInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TAudioGenreInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

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

function TEnumInterceptor<TEnum>.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TEnum;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TEnum>;
  result := Ord(TEnumHelp.Recast<TEnum>(value)).ToString;
end;

procedure TEnumInterceptor<TEnum>.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TEnum;
  v: TValue;
begin
  value := TEnumHelp.Cast<TEnum>(StrToIntDef(Arg, 0));
  v := v.From<TEnum>(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TStringDateTimeInterceptor }

constructor TStringDateTimeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TStringDateTimeInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  date: TDateTime;
begin
  date := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>;
  result := DateToStr(date);
end;

procedure TStringDateTimeInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
var
  ctx: TRTTIContext;
  datetime: TDateTime;
begin
  datetime := StrToDateDef(Arg, 0);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, datetime);
end;

{ TDeactivatedInterceptor }

constructor TDeactivatedInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TDeactivatedInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkDeactivated;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkDeactivated>;
  result := value.ToString;
end;

procedure TDeactivatedInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkDeactivated;
  v: TValue;
begin
  value := TVkDeactivated.Create(Arg);
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TAudioGenreInterceptor }

constructor TAudioGenreInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TAudioGenreInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkAudioGenre;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkAudioGenre>;
  result := value.ToString;
end;

procedure TAudioGenreInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkAudioGenre;
  v: TValue;
begin
  value := TVkAudioGenre.Create(StrToIntDef(Arg, 0));
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

end.

