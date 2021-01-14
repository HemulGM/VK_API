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
  end;

  TVkGroupCallbackServers = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkGroupCallbackServer>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkGroupCallbackServer> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGroupCallbackServers}

destructor TVkGroupCallbackServers.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupCallbackServer>(FItems);
  inherited;
end;

end.

