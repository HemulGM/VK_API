unit VK.Entity.Call;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common;

type
  TVkCall = class(TVkEntity)
  private
    FDuration: Integer;
    FInitiator_id: Integer;
    FReceiver_id: Integer;
    FState: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTime: TDateTime;
    FVideo: Boolean;
  public
    property Duration: Integer read FDuration write FDuration;
    property InitiatorId: Integer read FInitiator_id write FInitiator_id;
    property ReceiverId: Integer read FReceiver_id write FReceiver_id;
    property State: string read FState write FState;
    property Time: TDateTime read FTime write FTime;
    property Video: Boolean read FVideo write FVideo;
  end;

implementation

end.

