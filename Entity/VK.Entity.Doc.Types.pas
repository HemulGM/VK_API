unit VK.Entity.Doc.Types;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkDocType = class(TVkObject)
  private
    FCount: Integer;
    FName: string;
  public
    property Count: Integer read FCount write FCount;
    property Name: string read FName write FName;
  end;

  TVkDocTypes = TVkEntityList<TVkDocType>;

implementation

end.

