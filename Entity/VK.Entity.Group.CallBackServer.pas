unit VK.Entity.Group.CallBackServer;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkGroupCallbackServer = class(TVkObject)
  private
    FCreator_id: Integer;
    FSecret_key: string;
    FStatus: string;
    FTitle: string;
    FUrl: string;
  public
    property CreatorId: Integer read FCreator_id write FCreator_id;
    property SecretKey: string read FSecret_key write FSecret_key;
    property Status: string read FStatus write FStatus;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupCallbackServer;
  end;

  TVkGroupCallbackServers = class
  private
    FCount: Integer;
    FItems: TArray<TVkGroupCallbackServer>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkGroupCallbackServer> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupCallbackServers;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGroupCallbackServer}

function TVkGroupCallbackServer.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupCallbackServer.FromJsonString(AJsonString: string): TVkGroupCallbackServer;
begin
  result := TJson.JsonToObject<TVkGroupCallbackServer>(AJsonString)
end;

{TVkGroupCallbackServers}

destructor TVkGroupCallbackServers.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupCallbackServer>(FItems);
  inherited;
end;

function TVkGroupCallbackServers.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupCallbackServers.FromJsonString(AJsonString: string): TVkGroupCallbackServers;
begin
  result := TJson.JsonToObject<TVkGroupCallbackServers>(AJsonString)
end;

end.

