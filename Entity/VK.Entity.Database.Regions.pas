unit VK.Entity.Database.Regions;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkRegion = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRegion;
  end;

  TVkRegions = class
  private
    FCount: Integer;
    FItems: TArray<TVkRegion>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkRegion> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRegions;
  end;

implementation

uses
  VK.CommonUtils;

{TVkRegion}

function TVkRegion.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRegion.FromJsonString(AJsonString: string): TVkRegion;
begin
  result := TJson.JsonToObject<TVkRegion>(AJsonString)
end;

{TVkRegions}

destructor TVkRegions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkRegion>(FItems);
  inherited;
end;

function TVkRegions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRegions.FromJsonString(AJsonString: string): TVkRegions;
begin
  result := TJson.JsonToObject<TVkRegions>(AJsonString)
end;

end.

