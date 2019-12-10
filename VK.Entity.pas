unit VK.Entity;

interface

uses
  VK.Handler;

type
  TVKEntity = class
  private
    FHandler: TVKHandler;
    procedure SetHandler(const Value: TVKHandler);
  public
    constructor Create(AHandler: TVKHandler);
    property Handler: TVKHandler read FHandler write SetHandler;
  end;

implementation

{ TVKEntity }

constructor TVKEntity.Create(AHandler: TVKHandler);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TVKEntity.SetHandler(const Value: TVKHandler);
begin
  FHandler := Value;
end;

end.

