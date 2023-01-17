unit VK.Entity.Database.Chairs;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkChair = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkChairs = TVkEntityList<TVkChair>;

implementation

end.

