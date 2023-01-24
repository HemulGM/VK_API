unit VK.Entity.Database.Universities;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkUniversity = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkUniversities = TVkEntityList<TVkUniversity>;

implementation

end.

