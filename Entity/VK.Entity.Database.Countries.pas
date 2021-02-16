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
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Title: string read FTitle write FTitle;
  end;

  TVkCountries = TVkEntityList<TVkCountry>;

implementation

end.

