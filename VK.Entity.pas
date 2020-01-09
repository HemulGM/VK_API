unit VK.Entity;

interface

uses
  VK.Handler;

type
  TVKEntity = class
  private
    FHandler: TVKHandler;
    procedure SetHandler(const Value: TVKHandler);
    function GetVK: TObject;
  public
    constructor Create(AHandler: TVKHandler);
    property Handler: TVKHandler read FHandler write SetHandler;
    property VK: TObject read GetVK;
  end;

implementation

{ TVKEntity }

constructor TVKEntity.Create(AHandler: TVKHandler);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TVKEntity.GetVK: TObject;
begin
  Result := FHandler.Owner;
end;

procedure TVKEntity.SetHandler(const Value: TVKHandler);
begin
  FHandler := Value;
end;

end.

