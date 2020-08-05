unit VK.Entity.Database.Chairs;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkChair = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChair;
  end;

  TVkChairs = class
  private
    FCount: Extended;
    FItems: TArray<TVkChair>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TVkChair> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChairs;
  end;

implementation

{TVkChair}

function TVkChair.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChair.FromJsonString(AJsonString: string): TVkChair;
begin
  result := TJson.JsonToObject<TVkChair>(AJsonString)
end;

{TVkChairs}

destructor TVkChairs.Destroy;
var
  LitemsItem: TVkChair;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkChairs.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChairs.FromJsonString(AJsonString: string): TVkChairs;
begin
  result := TJson.JsonToObject<TVkChairs>(AJsonString)
end;

end.

