unit VK.Entity.Database.MetroStations;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkMetroStation = class(TVkObject)
  private
    FColor: string;
    FName: string;
  public
    property Color: string read FColor write FColor;
    property Name: string read FName write FName;
  end;

  TVkMetroStations = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkMetroStation>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkMetroStation> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkMetroStations}

destructor TVkMetroStations.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkMetroStation>(FItems);
  {$ENDIF}
  inherited;
end;

end.

