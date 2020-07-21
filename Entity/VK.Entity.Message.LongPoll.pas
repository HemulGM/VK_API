unit VK.Entity.Message.LongPoll;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkUserLongpoll = class
  private
    FKey: string;
    FServer: string;
    FTs: Int64;
  public
    property Key: string read FKey write FKey;
    property Server: string read FServer write FServer;
    property Ts: Int64 read FTs write FTs;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUserLongpoll;
  end;

implementation

{TVkUserLongpoll}

function TVkUserLongpoll.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUserLongpoll.FromJsonString(AJsonString: string): TVkUserLongpoll;
begin
  result := TJson.JsonToObject<TVkUserLongpoll>(AJsonString)
end;

end.

