unit VK.Entity.Database.Countries;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkCountry = class
  private
    FId: Integer;
    FTitle: string;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCountry;
  end;

  TVkCountries = class
  private
    FCount: Integer;
    FItems: TArray<TVkCountry>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkCountry> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCountries;
  end;

implementation

{TVkCountry}

function TVkCountry.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountry.FromJsonString(AJsonString: string): TVkCountry;
begin
  result := TJson.JsonToObject<TVkCountry>(AJsonString)
end;

{TVkCountries}

destructor TVkCountries.Destroy;
var
  LitemsItem: TVkCountry;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkCountries.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountries.FromJsonString(AJsonString: string): TVkCountries;
begin
  result := TJson.JsonToObject<TVkCountries>(AJsonString)
end;

end.

