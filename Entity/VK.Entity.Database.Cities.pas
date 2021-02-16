unit VK.Entity.Database.Cities;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkCity = class(TVkObject)
  private
    FTitle: string;
    FImportant: Boolean;
    FArea: string;
    FRegion: string;
  public
    /// <summary>
    /// Идентификатор города
    /// </summary>
    property Id;
    /// <summary>
    /// Название города
    /// </summary>
    property Title: string read FTitle write FTitle;
    property Important: Boolean read FImportant write FImportant;
    property Area: string read FArea write FArea;
    property Region: string read FRegion write FRegion;
  end;

  TVkCities = TVkEntityList<TVkCity>;

implementation

end.

