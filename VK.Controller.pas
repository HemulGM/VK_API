unit VK.Controller;

interface

uses
  VK.Handler;

type
  TVkController = class
  private
    FHandler: TVkHandler;
    procedure SetHandler(const Value: TVkHandler);
    function GetVK: TObject;
  public
    constructor Create(AHandler: TVkHandler);
    property Handler: TVkHandler read FHandler write SetHandler;
    property VK: TObject read GetVK;
  end;

implementation

{ TVkController }

constructor TVkController.Create(AHandler: TVkHandler);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TVkController.GetVK: TObject;
begin
  Result := FHandler.Owner;
end;

procedure TVkController.SetHandler(const Value: TVkHandler);
begin
  FHandler := Value;
end;

end.

