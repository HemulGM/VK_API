unit VK.Entity.Call;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCall = class(TVkEntity)
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
  end;

implementation

end.

