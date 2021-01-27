unit VK.Wrap.Interceptors;

interface

uses
  Generics.Collections, System.SysUtils, TypInfo, System.Types, System.RTTI, Rest.Json, REST.JsonReflect,
  REST.Json.Interceptors, VK.Types;

type
  TEnumHelp = record
    type
      ETEnumHelpError = class(Exception);
    class function Cast<TEnum>(const Value: Integer): TEnum; static;
    class function Recast<TEnum>(const Value: TEnum): Integer; static;
  end;

  TIntBooleanInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
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

  TPeerTypeInterceptor = class(TEnumInterceptor<TVkPeerType>)
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

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

  TAttachmentTypeInterceptor = class(TJSONInterceptor)
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TKeyboardButtonColorInterceptor = class(TEnumInterceptor<TVkKeyboardButtonColor>)
    constructor Create; reintroduce;
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
var
  ctx: TRTTIContext;
  value: Boolean;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<Boolean>;
  result := IfThen(value, '1', '0');
end;

procedure TIntBooleanInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: Boolean;
begin
  value := Arg = '1';
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, value);
end;

{ TPeerTypeInterceptor }

constructor TPeerTypeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TPeerTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkPeerType;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkPeerType>;
  result := value.ToString;
end;

procedure TPeerTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkPeerType;
  v: TValue;
begin
  value := TVkPeerType.Create(Arg);
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TAttachmentTypeInterceptor }

constructor TAttachmentTypeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TAttachmentTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkAttachmentType;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkAttachmentType>;
  result := value.ToString;
end;

procedure TAttachmentTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkAttachmentType;
  v: TValue;
begin
  value := TVkAttachmentType.Create(Arg);
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

{ TDocumentTypeInterceptor }

constructor TDocumentTypeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ TKeyboardButtonColorInterceptor }

constructor TKeyboardButtonColorInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TKeyboardButtonColorInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRTTIContext;
  value: TVkKeyboardButtonColor;
begin
  value := ctx.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkKeyboardButtonColor>;
  result := value.ToString;
end;

procedure TKeyboardButtonColorInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRTTIContext;
  value: TVkKeyboardButtonColor;
  v: TValue;
begin
  value := TVkKeyboardButtonColor.Create(Arg);
  v := v.From(value);
  ctx.GetType(Data.ClassType).GetField(Field).SetValue(Data, v);
end;

end.

