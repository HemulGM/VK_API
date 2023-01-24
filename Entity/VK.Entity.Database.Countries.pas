unit VK.Entity.Database.Countries;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkCountry = class(TVkObject)
  private
    FTitle: string;
  public
    /// <summary>
    /// Идентификатор страны
    /// </summary>
    property Id;
    /// <summary>
    /// Название страны
    /// </summary>
    property Title: string read FTitle write FTitle;
  end;

  TVkCountries = TVkEntityList<TVkCountry>;

implementation

end.

