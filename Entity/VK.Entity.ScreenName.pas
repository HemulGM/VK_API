unit VK.Entity.ScreenName;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkScreenNameType = class
  private
    FObject_id: Integer;
    FType: string;
  public
    property ObjectId: Integer read FObject_id write FObject_id;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkScreenNameType;
  end;

implementation

{TVkScreenNameType}

function TVkScreenNameType.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkScreenNameType.FromJsonString(AJsonString: string): TVkScreenNameType;
begin
  result := TJson.JsonToObject<TVkScreenNameType>(AJsonString)
end;

end.

