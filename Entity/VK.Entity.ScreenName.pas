unit VK.Entity.ScreenName;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkScreenNameType = class(TVkEntity)
  private
    FObject_id: Integer;
    FType: string;
  public
    property ObjectId: Integer read FObject_id write FObject_id;
    property &Type: string read FType write FType;
  end;

implementation

end.

