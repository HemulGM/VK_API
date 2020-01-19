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
    property id: Extended read FId write FId;
    property owner_id: integer read FOwner_id write FOwner_id;
    property photo_130: string read FPhoto_130 write FPhoto_130;
    property photo_604: string read FPhoto_604 write FPhoto_604;
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

