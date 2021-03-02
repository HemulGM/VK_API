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

  /// <summary>
  /// TEnum <-> int
  /// </summary>
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

  /// <summary>
  /// 20131224 -> TDateTime
  /// </summary>
  TIntDateTimeInterceptor = class(TJSONInterceptorStringToString)
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

  TGroupAccessInterceptor = class(TEnumInterceptor<TVkGroupAccess>)
  end;

  TGroupAdminLevelInterceptor = class(TEnumInterceptor<TVkGroupAdminLevel>)
  end;

  TAgeLimitsInterceptor = class(TEnumInterceptor<TVkAgeLimits>)
  end;

  TGroupMainSectionInterceptor = class(TEnumInterceptor<TVkGroupMainSection>)
  end;

  TConversationDisableReasonInterceptor = class(TEnumInterceptor<TVkConversationDisableReason>)
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

  TGroupStatusTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TGroupTypeInterceptor = class(TJSONInterceptorStringToString)
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

  TKeyboardActionTypeInterceptor = class(TEnumInterceptor<TVkKeyboardActionType>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TChatStateInterceptor = class(TEnumInterceptor<TVkChatState>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TStreamStatIntervalInterceptor = class(TEnumInterceptor<TVkStreamStatInterval>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TVideoTypeInterceptor = class(TEnumInterceptor<TVkVideoType>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TLiveStatusInterceptor = class(TEnumInterceptor<TVkLiveStatus>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

uses
  System.StrUtils;

{ TEnumHelp }

class function TEnumHelp.Cast<TEnum>(const Value: Integer): TEnum;
begin
  case SizeOf(TEnum) of
    1:
      PByte(@Result)^ := Value;
    2:
      PWord(@Result)^ := Value;
    4:
      PCardinal(@Result)^ := Value;
  end;
end;

class function TEnumHelp.Recast<TEnum>(const Value: TEnum): Integer;
begin
  case SizeOf(TEnum) of
    1:
      Result := PByte(@Value)^;
    2:
      Result := PWord(@Value)^;
    4:
      Result := PCardinal(@Value)^;
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
  Result := Ord(TEnumHelp.Recast<TEnum>(RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TEnum>)).ToString;
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

{ TIntDateTimeInterceptor }

function TIntDateTimeInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  Dt: TDateTime;
begin
  Dt := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>;
  Result := FormatDateTime('YYYYMMDD', Dt);
end;

procedure TIntDateTimeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  if Arg.Length = 8 then
    Arg := Arg.Chars[6] + Arg.Chars[7] +
      FormatSettings.DateSeparator + Arg.Chars[4] + Arg.Chars[5] +
      FormatSettings.DateSeparator + Arg.Substring(0, 4);
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

{ TGroupStatusTypeInterceptor }

function TGroupStatusTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkGroupStatusType>.ToString;
end;

procedure TGroupStatusTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkGroupStatusType.Create(Arg)));
end;

{ TGroupTypeInterceptor }

function TGroupTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkGroupType>.ToString;
end;

procedure TGroupTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkGroupType.Create(Arg)));
end;

{ TKeyboardActionTypeInterceptor }

function TKeyboardActionTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkKeyboardActionType>.ToString;
end;

procedure TKeyboardActionTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkKeyboardActionType.Create(Arg)));
end;

{ TChatStateInterceptor }

function TChatStateInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkChatState>.ToString;
end;

procedure TChatStateInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkChatState.Create(Arg)));
end;

{ TStreamStatIntervalInterceptor }

function TStreamStatIntervalInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkStreamStatInterval>.ToString;
end;

procedure TStreamStatIntervalInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkStreamStatInterval.Create(Arg)));
end;

{ TVideoTypeInterceptor }

function TVideoTypeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkVideoType>.ToString;
end;

procedure TVideoTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkVideoType.Create(Arg)));
end;

{ TLiveStatusInterceptor }

function TLiveStatusInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TVkLiveStatus>.ToString;
end;

procedure TLiveStatusInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TVkLiveStatus.Create(Arg)));
end;

end.

