unit VK.Params;

interface

uses
  System.Classes, System.JSON, System.Generics.Collections, VK.Entity.Common;

type
  TJSONParam = class
  private
    FJSON: TJSONObject;
    procedure SetJSON(const Value: TJSONObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Key: string; const Value: string): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Integer): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Extended): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Boolean): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TDateTime; Format: string = ''): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TJSONValue): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TJSONParam): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TVkEntity): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<string>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam; overload; virtual;
    function Add<T: TVkEntity, constructor>(const Key: string; Value: TArray<T>): TJSONParam; overload;
    function GetOrCreateObject(const Name: string): TJSONObject;
    function GetOrCreate<T: TJSONValue, constructor>(const Name: string): T;
    procedure Delete(const Key: string); virtual;
    procedure Clear; virtual;
    property JSON: TJSONObject read FJSON write SetJSON;
    function ToJsonString(FreeObject: Boolean = False): string; virtual;
  end;

implementation

uses
  System.SysUtils;

{ Fetch }

type
  Fetch<T> = class
    type
      TFetchProc = reference to procedure(const Element: T);
  public
    class procedure All(const Items: TArray<T>; Proc: TFetchProc);
  end;

{ Fetch<T> }

class procedure Fetch<T>.All(const Items: TArray<T>; Proc: TFetchProc);
var
  Item: T;
begin
  for Item in Items do
    Proc(Item);
end;

{ TJSONParam }

function TJSONParam.Add(const Key, Value: string): TJSONParam;
begin
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONValue): TJSONParam;
begin
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONParam): TJSONParam;
begin
  Add(Key, TJSONValue(Value.JSON.Clone));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TDateTime; Format: string): TJSONParam;
begin
  if Format.IsEmpty then
    Format := FormatSettings.ShortDateFormat;
  Add(Key, FormatDateTime(Format, Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Boolean): TJSONParam;
begin
  Add(Key, TJSONBool.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Integer): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Extended): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam;
var
  JArr: TJSONArray;
begin
  JArr := TJSONArray.Create;
  Fetch<TJSONValue>.All(Value, JArr.AddElement);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add<T>(const Key: string; Value: TArray<T>): TJSONParam;
var
  JArr: TJSONArray;
  Item: T;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.AddElement(Item.ToJsonObject);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam;
var
  JArr: TJSONArray;
  Item: TJSONParam;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
  begin
    JArr.AddElement(Item.JSON);
    Item.JSON := nil;
  end;

  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<string>): TJSONParam;
var
  JArr: TJSONArray;
  Item: string;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.Add(Item);

  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TVkEntity): TJSONParam;
begin
  Add(Key, Value.ToJsonString);
  Result := Self;
end;

procedure TJSONParam.Clear;
begin
  FJSON.Free;
  FJSON := TJSONObject.Create;
end;

constructor TJSONParam.Create;
begin
  FJSON := TJSONObject.Create;
end;

procedure TJSONParam.Delete(const Key: string);
var
  Item: TJSONPair;
begin
  Item := FJSON.RemovePair(Key);
  if Assigned(Item) then
    Item.Free;
end;

destructor TJSONParam.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

function TJSONParam.GetOrCreate<T>(const Name: string): T;
begin
  if not FJSON.TryGetValue<T>(Name, Result) then
  begin
    Result := T.Create;
    FJSON.AddPair(Name, Result);
  end;
end;

function TJSONParam.GetOrCreateObject(const Name: string): TJSONObject;
begin
  Result := GetOrCreate<TJSONObject>(Name);
end;

procedure TJSONParam.SetJSON(const Value: TJSONObject);
begin
  FJSON := Value;
end;

function TJSONParam.ToJsonString(FreeObject: Boolean): string;
begin
  Result := FJSON.ToJSON;
  if FreeObject then
    Free;
end;

end.

