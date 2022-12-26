unit VK.Entity.Group.CallBackServer;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List,
  VK.Types;

type
  TVkGroupCallbackServer = class(TVkObject)
  private
    FCreator_id: TVkPeerId;
    FSecret_key: string;
    FStatus: string;
    FTitle: string;
    FUrl: string;
  public
    property CreatorId: TVkPeerId read FCreator_id write FCreator_id;
    property SecretKey: string read FSecret_key write FSecret_key;
    property Status: string read FStatus write FStatus;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
  end;

  TVkGroupCallbackServers = TVkEntityList<TVkGroupCallbackServer>;

implementation

end.

