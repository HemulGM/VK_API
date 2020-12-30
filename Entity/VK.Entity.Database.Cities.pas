unit VK.Entity.Database.Cities;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCity = class(TVkObject)
  private
    FTitle: string;
    FImportant: Boolean;
    FArea: string;
    FRegion: string;
  public
    property Title: string read FTitle write FTitle;
    property Important: Boolean read FImportant write FImportant;
    property Area: string read FArea write FArea;
    property Region: string read FRegion write FRegion;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCity;
  end;

  TVkCities = class
  private
    FCount: Integer;
    FItems: TArray<TVkCity>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkCity> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCities;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCity}

function TVkCity.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCity.FromJsonString(AJsonString: string): TVkCity;
begin
  result := TJson.JsonToObject<TVkCity>(AJsonString)
end;

{TVkCities}

destructor TVkCities.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCity>(FItems);
  inherited;
end;

function TVkCities.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCities.FromJsonString(AJsonString: string): TVkCities;
begin
  result := TJson.JsonToObject<TVkCities>(AJsonString)
end;

end.

