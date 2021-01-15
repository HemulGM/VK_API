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
  end;

  TVkRegions = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkRegion>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkRegion> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkRegions}

destructor TVkRegions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkRegion>(FItems);
  inherited;
end;

end.

