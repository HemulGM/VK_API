unit VK.Entity.Call;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkCall = class
  private
    FDuration: Integer;
    FInitiator_id: Integer;
    FReceiver_id: Integer;
    FState: string;
    FTime: Int64;
    FVideo: Boolean;
  public
    property Duration: Integer read FDuration write FDuration;
    property InitiatorId: Integer read FInitiator_id write FInitiator_id;
    property ReceiverId: Integer read FReceiver_id write FReceiver_id;
    property State: string read FState write FState;
    property Time: Int64 read FTime write FTime;
    property Video: Boolean read FVideo write FVideo;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCall;
  end;

implementation

{TVkCall}

function TVkCall.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCall.FromJsonString(AJsonString: string): TVkCall;
begin
  result := TJson.JsonToObject<TVkCall>(AJsonString)
end;

end.

