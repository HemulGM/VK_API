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

function TVkCountries.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountries.FromJsonString(AJsonString: string): TVkCountries;
begin
  result := TJson.JsonToObject<TVkCountries>(AJsonString)
end;

end.

