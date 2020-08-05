unit VK.Entity.Database.MetroStations;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkMetroStation = class
  private
    FColor: string;
    FId: Integer;
    FName: string;
  public
    property Color: string read FColor write FColor;
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMetroStation;
  end;

  TVkMetroStations = class
  private
    FCount: Integer;
    FItems: TArray<TVkMetroStation>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkMetroStation> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMetroStations;
  end;

implementation

{TVkMetroStation}

function TVkMetroStation.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMetroStation.FromJsonString(AJsonString: string): TVkMetroStation;
begin
  result := TJson.JsonToObject<TVkMetroStation>(AJsonString)
end;

{TVkMetroStations}

destructor TVkMetroStations.Destroy;
var
  LitemsItem: TVkMetroStation;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkMetroStations.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMetroStations.FromJsonString(AJsonString: string): TVkMetroStations;
begin
  result := TJson.JsonToObject<TVkMetroStations>(AJsonString)
end;

end.

