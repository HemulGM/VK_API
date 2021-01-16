unit VK.Entity.Longpoll;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkLongpollData = class(TVkEntity)
  private
    FKey: string;
    FServer: string;
    FTs: string;
  public
    property Key: string read FKey write FKey;
    property Server: string read FServer write FServer;
    property Ts: string read FTs write FTs;
  end;

implementation

end.

