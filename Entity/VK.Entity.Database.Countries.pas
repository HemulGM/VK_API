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

uses
  VK.CommonUtils;

{TVkCountries}

destructor TVkCountries.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkCountry>(FItems);
  {$ENDIF}
  inherited;
end;

end.

