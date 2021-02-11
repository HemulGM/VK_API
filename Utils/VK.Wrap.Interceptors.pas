unit VK.Wrap.Interceptors;

interface

uses
  System.SysUtils, TypInfo, System.Types, System.RTTI, REST.JsonReflect,
  REST.Json.Interceptors, VK.Types;

type
  TEnumHelp = record
    type
      ETEnumHelpError = class(Exception);
    class function Cast<TEnum>(const Value: Integer): TEnum; static;
    class function Recast<TEnum>(const Value: TEnum): Integer; static;
  end;

  //Base Interceptor classes

  TJSONInterceptorStringToString = class(TJSONInterceptor)
    constructor Create; reintroduce;
  protected
    RTTI: TRttiContext;
  end;

  TEnumInterceptor<TEnum> = class(TJSONInterceptorStringToString)
    constructor Create; reintroduce;
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  ///  0 -> False, 1 -> True
  /// </summary>
  TIntBooleanInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// StrToDate, DateToStr
  /// </summary>
  TStringDateTimeInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TBirthDateVisibilityInterceptor = class(TEnumInterceptor<TVkBirthDateVisibility>)
  end;

  TPoliticalInterceptor = class(TEnumInterceptor<TVkPolitical>)
  end;

  TPlatformInterceptor = class(TEnumInterceptor<TVkPlatform>)
  end;

  TSexInterceptor = class(TEnumInterceptor<TVkSex>)
  end;

  TRelationInterceptor = class(TEnumInterceptor<TVkRelation>)
  end;

  TNameRequestStatusInterceptor = class(TEnumInterceptor<TVkNameRequestStatus>)
  end;

  TDocumentTypeInterceptor = class(TEnumInterceptor<TVkDocumentType>)
  end;

  TOrderStatusInterceptor = class(TEnumInterceptor<TVkOrderStatus>)
  end;

  TProductAvailabilityInterceptor = class(TEnumInterceptor<TVkProductAvailability>)
  end;

  TPeerTypeInterceptor = class(TJSONInterceptorStringToString)
  public
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
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TMessageActionTypeInterceptor = class(TEnumInterceptor<TVkMessageActionType>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

uses
  System.StrUtils;

{ TEnumHelp }

class function TEnumHelp.Cast<TEnum>(const Value: Integer): TEnum;
var
  Inf: PTypeInfo;
  Data: PTypeData;
begin
  Inf := PTypeInfo(TypeInfo(TEnum));
  if (Inf = nil) or (Inf^.Kind <> tkEnumeration) then
    raise ETEnumHelpError.Create('Not an enumeration type');
  Data := GetTypeData(Inf);
  if (Value < Data^.MinValue) then
    raise ETEnumHelpError.CreateFmt('%d is below min value [%d]', [Value, Data^.MinValue])
  else if (Value > Data^.MaxValue) then
    raise ETEnumHelpError.CreateFmt('%d is above max value [%d]', [Value, Data^.MaxValue]);
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
begin
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

{ TIntBooleanInterceptor }

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

