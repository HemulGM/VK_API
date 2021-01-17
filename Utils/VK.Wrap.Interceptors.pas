unit VK.Wrap.Interceptors;

interface

uses
  Generics.Collections, System.SysUtils, TypInfo, System.Types, System.RTTI, Rest.Json, REST.JsonReflect,
  REST.Json.Interceptors, VK.Types;

type
  TEnumHelp<TEnum> = record
    type
      ETEnumHelpError = class(Exception);
    class function Cast(const Value: Integer): TEnum; static;
    class function Recast(const Value: TEnum): Integer; static;
  end;

  TStringDateTimeInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TEnumInterceptor<TEnum> = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TBirthDateVisibilityInterceptor1 = TEnumInterceptor<TVkBirthDateVisibility>;

  TSexInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TBirthDateVisibilityInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TRelationInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TNameRequestStatusInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

uses
  VK.Entity.ProfileInfo;

{ TEnumHelp }

class function TEnumHelp<TEnum>.Cast(const Value: Integer): TEnum;
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

class function TEnumHelp<TEnum>.Recast(const Value: TEnum): Integer;
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
      pByte(@Value)^ := Result;
    2:
      pWord(@Value)^ := Result;
    4:
      pCardinal(@Value)^ := Result;
  end;
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

{ TSexInterceptor }

constructor TSexInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TSexInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkSex;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkSex>;
  result := Ord(value).ToString;
end;

procedure TSexInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkSex;
  v: TValue;
begin
  value := TVkSex(StrToIntDef(Arg, 0));
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TEnumInterceptor<TEnum> }

constructor TEnumInterceptor<TEnum>.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TEnumInterceptor<TEnum>.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TEnum;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TEnum>;
  result := Ord(TEnumHelp<TEnum>.Recast(value)).ToString;
end;

procedure TEnumInterceptor<TEnum>.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TEnum;
  v: TValue;
begin
  value := TEnumHelp<TEnum>.Cast(StrToIntDef(Arg, 0));
  v := v.From<TEnum>(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TBirthDateVisibilityInterceptor }

constructor TBirthDateVisibilityInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TBirthDateVisibilityInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkBirthDateVisibility;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkBirthDateVisibility>;
  result := Ord(value).ToString;
end;

procedure TBirthDateVisibilityInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkBirthDateVisibility;
  v: TValue;
begin
  value := TVkBirthDateVisibility(StrToIntDef(Arg, 0));
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TRelationInterceptor }

constructor TRelationInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TRelationInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkRelation;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkRelation>;
  result := Ord(value).ToString;
end;

procedure TRelationInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkRelation;
  v: TValue;
begin
  value := TVkRelation(StrToIntDef(Arg, 0));
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TNameRequestStatusInterceptor }

constructor TNameRequestStatusInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TNameRequestStatusInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkNameRequestStatus;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkNameRequestStatus>;
  result := value.ToString;
end;

procedure TNameRequestStatusInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkNameRequestStatus;
  v: TValue;
begin
  value := TVkNameRequestStatus.Create(Arg);
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

end.

