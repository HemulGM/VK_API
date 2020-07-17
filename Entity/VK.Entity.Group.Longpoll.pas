unit VK.Entity.Group.Longpoll;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkGroupLongpoll = class
  private
    FKey: string;
    FServer: string;
    FTs: string;
  public
    property Key: string read FKey write FKey;
    property Server: string read FServer write FServer;
    property Ts: string read FTs write FTs;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupLongpoll;
  end;

implementation

{TVkGroupLongpoll}

function TVkGroupLongpoll.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupLongpoll.FromJsonString(AJsonString: string): TVkGroupLongpoll;
begin
  result := TJson.JsonToObject<TVkGroupLongpoll>(AJsonString)
end;

end.

