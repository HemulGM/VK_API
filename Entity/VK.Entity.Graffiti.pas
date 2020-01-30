unit VK.Entity.Graffiti;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkGraffiti = class
  private
    FId: Extended;
    FPhoto_604: string;
    FOwner_id: integer;
    FPhoto_130: string;
  public
    property Id: Extended read FId write FId;
    property OwnerId: integer read FOwner_id write FOwner_id;
    property Photo130: string read FPhoto_130 write FPhoto_130;
    property Photo604: string read FPhoto_604 write FPhoto_604;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGraffiti;
  end;

implementation

{TVkGraffiti}

function TVkGraffiti.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGraffiti.FromJsonString(AJsonString: string): TVkGraffiti;
begin
  result := TJson.JsonToObject<TVkGraffiti>(AJsonString)
end;

end.

