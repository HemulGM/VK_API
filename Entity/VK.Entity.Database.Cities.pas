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
  end;

  TVkCities = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkCity>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkCity> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCities}

destructor TVkCities.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkCity>(FItems);
  {$ENDIF}
  inherited;
end;

end.

