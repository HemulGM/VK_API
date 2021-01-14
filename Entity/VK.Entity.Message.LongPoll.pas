unit VK.Entity.Message.LongPoll;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkUserLongpoll = class(TVkEntity)
  private
    FKey: string;
    FServer: string;
    FTs: Int64;
  public
    property Key: string read FKey write FKey;
    property Server: string read FServer write FServer;
    property Ts: Int64 read FTs write FTs;
  end;

implementation

end.

