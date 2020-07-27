unit VK.Controller;

interface

uses
  VK.Handler, System.Json;

type
  TVkController = class
  private
    FHandler: TVkHandler;
    FGenerateException: Boolean;
    procedure SetHandler(const Value: TVkHandler);
    function GetVK: TObject;
    procedure SetGenerateException(const Value: Boolean);
  protected
    function GetValue<T>(const Response, Field: string; var Value: T): Boolean;
  public
    constructor Create(AHandler: TVkHandler);
    property Handler: TVkHandler read FHandler write SetHandler;
    property VK: TObject read GetVK;
    property GenerateException: Boolean read FGenerateException write SetGenerateException;
  end;

implementation

{ TVkController }

constructor TVkController.Create(AHandler: TVkHandler);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TVkController.GetValue<T>(const Response, Field: string; var Value: T): Boolean;
var
  RespJSON: TJSONValue;
begin
  try
    RespJSON := TJSONObject.ParseJSONValue(Response);
    try
      Result := RespJSON.TryGetValue(Field, Value);
    finally
      RespJSON.Free;
    end;
  except
    Result := False;
  end;
end;

function TVkController.GetVK: TObject;
begin
  Result := FHandler.Owner;
end;

procedure TVkController.SetGenerateException(const Value: Boolean);
begin
  FGenerateException := Value;
end;

procedure TVkController.SetHandler(const Value: TVkHandler);
begin
  FHandler := Value;
end;

end.

