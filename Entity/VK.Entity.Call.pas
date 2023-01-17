unit VK.Entity.Call;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Wrap.Interceptors, VK.Types;

type
  TVkCall = class(TVkEntity)
  private
    FDuration: Int64;
    FInitiator_id: TVkPeerId;
    FReceiver_id: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TCallStateInterceptor)]
    FState: TVkCallState;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FTime: TDateTime;
    FVideo: Boolean;
    FAccess_key: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Duration: Int64 read FDuration write FDuration;
    property InitiatorId: TVkPeerId read FInitiator_id write FInitiator_id;
    property ReceiverId: TVkPeerId read FReceiver_id write FReceiver_id;
    property State: TVkCallState read FState write FState;
    property Time: TDateTime read FTime write FTime;
    property Video: Boolean read FVideo write FVideo;
  end;

  TVkCallStartInfo = class(TVkEntity)
  private
    FJoin_link: string;
    FCall_id: Int64;
  public
    property JoinLink: string read FJoin_link write FJoin_link;
    property CallId: Int64 read FCall_id write FCall_id;
  end;

implementation

end.

