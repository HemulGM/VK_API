unit VK.FakeAndroidProto;

interface

uses
  SysUtils, Classes, Generics.Collections, pbInput, pbOutput, pbPublic,
  uAbstractProtoBufClasses;

type
  TAndroidChekin = class(TAbstractProtoBufClass)
  public
    const
      Tag_CellOperator = 1;
      Tag_Roaming = 2;
      Tag_SimOperator = 3;
      Tag_Type = 4;
  private
    FType: string;
    FSimOperator: string;
    FRoaming: string;
    FCellOperator: string;
    procedure SetSimOperator(const Index: Integer; const Value: string);
    procedure SetRoaming(const Index: Integer; const Value: string);
    procedure SetCellOperator(const Index: Integer; const Value: string);
    procedure SetType(const Index: Integer; const Value: string);
  protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public
    property CellOperator: string index Tag_CellOperator read FCellOperator write SetCellOperator;
    property&Type: string index Tag_Type read FType write SetType;
    property SimOperator: string index Tag_SimOperator read FSimOperator write SetSimOperator;
    property Roaming: string index Tag_Roaming read FRoaming write SetRoaming;
  end;

  TAndroidCheckinRequest = class(TAbstractProtoBufClass)
  public
    const
      Tag_Checkin = 1;
      Tag_Digest = 2;
      Tag_Locale = 3;
      Tag_LoggingId = 4;
      Tag_Meid = 5;
      Tag_OtaCerts = 6;
      Tag_TimeZone = 7;
      Tag_Version = 8;
  private
    FOtaCerts: TList<string>;
    FVersion: Integer;
    FTimeZone: string;
    FLoggingId: Int64;
    FMeid: string;
    FCheckin: TAndroidChekin;
    FLocale: string;
    FDigest: string;
    procedure SetVersion(const Index, Value: Integer);
    procedure SetTimeZone(const Index: Integer; const Value: string);
    procedure SetLoggingId(const Index: Integer; const Value: Int64);
    procedure SetMeid(const Index: Integer; const Value: string);
    procedure SetLocale(const Index: Integer; const Value: string);
    procedure SetDigest(const Index: Integer; const Value: string);
  protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Meid: string index Tag_Meid read FMeid write SetMeid;
    property Digest: string index Tag_Digest read FDigest write SetDigest;
    property Locale: string index Tag_Locale read FLocale write SetLocale;
    property TimeZone: string index Tag_TimeZone read FTimeZone write SetTimeZone;
    property LoggingId: Int64 index Tag_LoggingId read FLoggingId write SetLoggingId;
    property Version: Integer index Tag_Version read FVersion write SetVersion default 3;
    property Checkin: TAndroidChekin index Tag_Checkin read FCheckin write FCheckin;
    property OtaCerts: TList<string> index Tag_OtaCerts read FOtaCerts;
  end;

implementation

{ TAndroidChekin }

function TAndroidChekin.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean;
begin
  Result := inherited;
end;

procedure TAndroidChekin.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
begin
  inherited;
  if FieldHasValue[Tag_CellOperator] then
    ProtoBuf.writeString(Tag_CellOperator, FCellOperator);
  if FieldHasValue[Tag_Roaming] then
    ProtoBuf.writeString(Tag_Roaming, FRoaming);
  if FieldHasValue[Tag_SimOperator] then
    ProtoBuf.writeString(Tag_SimOperator, FSimOperator);
  if FieldHasValue[Tag_Type] then
    ProtoBuf.writeString(Tag_Type, FType);
end;

procedure TAndroidChekin.SetCellOperator(const Index: Integer; const Value: string);
begin
  FCellOperator := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidChekin.SetRoaming(const Index: Integer; const Value: string);
begin
  FRoaming := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidChekin.SetSimOperator(const Index: Integer; const Value: string);
begin
  FSimOperator := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidChekin.SetType(const Index: Integer; const Value: string);
begin
  FType := Value;
  FieldHasValue[Index] := True;
end;

{ TAndroidCheckinRequest }

constructor TAndroidCheckinRequest.Create;
begin
  inherited;
  FOtaCerts := TList<string>.Create;
end;

destructor TAndroidCheckinRequest.Destroy;
begin
  FOtaCerts.Free;
  inherited;
end;

function TAndroidCheckinRequest.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean;
begin
  Result := inherited;
end;

procedure TAndroidCheckinRequest.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
var
  tmpBuf: TProtoBufOutput;
  i: Integer;
begin
  inherited;
  tmpBuf := TProtoBufOutput.Create;
  try
    if FieldHasValue[Tag_Version] then
      ProtoBuf.writeSInt32(Tag_Version, FVersion);
    if FieldHasValue[Tag_Digest] then
      ProtoBuf.writeString(Tag_Digest, FDigest);
    if FieldHasValue[Tag_TimeZone] then
      ProtoBuf.writeString(Tag_TimeZone, FTimeZone);
    if FieldHasValue[Tag_LoggingId] then
      ProtoBuf.writeInt64(Tag_LoggingId, FLoggingId);
    if FieldHasValue[Tag_Meid] then
      ProtoBuf.writeString(Tag_Meid, FMeid);
    if FieldHasValue[Tag_Locale] then
      ProtoBuf.writeString(Tag_Locale, FLocale);
    if FieldHasValue[Tag_OtaCerts] then
      for i := 0 to FOtaCerts.Count - 1 do
        ProtoBuf.writeString(Tag_OtaCerts, FOtaCerts[i]);
  finally
    tmpBuf.Free
  end;
end;

procedure TAndroidCheckinRequest.SetDigest(const Index: Integer; const Value: string);
begin
  FDigest := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidCheckinRequest.SetLocale(const Index: Integer; const Value: string);
begin
  FLocale := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidCheckinRequest.SetLoggingId(const Index: Integer; const Value: Int64);
begin
  FLoggingId := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidCheckinRequest.SetMeid(const Index: Integer; const Value: string);
begin
  FMeid := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidCheckinRequest.SetTimeZone(const Index: Integer; const Value: string);
begin
  FTimeZone := Value;
  FieldHasValue[Index] := True;
end;

procedure TAndroidCheckinRequest.SetVersion(const Index, Value: Integer);
begin
  FVersion := Value;
  FieldHasValue[Index] := True;
end;

end.

