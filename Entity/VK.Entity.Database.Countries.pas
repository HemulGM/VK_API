unit VK.Entity.Database.Countries;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCountry = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkCountries = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkCountry>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkCountry> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

{TVkCountries}

destructor TVkCountries.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  LitemsItem: TVkCountry;
{$ENDIF}
begin
{$IFNDEF AUTOREFCOUNT}
  for LitemsItem in FItems do
    LitemsItem.Free;
{$ENDIF}
  inherited;
end;

end.

