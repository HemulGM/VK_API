unit VK.Entity.PushSettings;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkConversation = class
  private
    FDisabled_until: Extended;
    FPeer_id: Extended;
    FSound: Extended;
  public
    property disabled_until: Extended read FDisabled_until write FDisabled_until;
    property peer_id: Extended read FPeer_id write FPeer_id;
    property sound: Extended read FSound write FSound;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversation;
  end;

  TVkConversations = class
  private
    FCount: Extended;
    FItems: TArray<TVkConversation>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TVkConversation> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversations;
  end;

  TVkPushSettingsItem = class
  private
    FApp_request: TArray<string>;
    FBirthday: TArray<string>;
    FChat: TArray<string>;
    FComment: TArray<string>;
    FEvent_soon: TArray<string>;
    FFriend: TArray<string>;
    FFriend_accepted: TArray<string>;
    FFriend_found: TArray<string>;
    FGroup_accepted: TArray<string>;
    FGroup_invite: TArray<string>;
    FLike: TArray<string>;
    FMention: TArray<string>;
    FMsg: TArray<string>;
    FNew_post: TArray<string>;
    FReply: TArray<string>;
    FRepost: TArray<string>;
    FSdk_open: TArray<string>;
    FTag_photo: TArray<string>;
    FWall_post: TArray<string>;
    FWall_publish: TArray<string>;
  public
    property app_request: TArray<string> read FApp_request write FApp_request;
    property birthday: TArray<string> read FBirthday write FBirthday;
    property chat: TArray<string> read FChat write FChat;
    property comment: TArray<string> read FComment write FComment;
    property event_soon: TArray<string> read FEvent_soon write FEvent_soon;
    property friend: TArray<string> read FFriend write FFriend;
    property friend_accepted: TArray<string> read FFriend_accepted write FFriend_accepted;
    property friend_found: TArray<string> read FFriend_found write FFriend_found;
    property group_accepted: TArray<string> read FGroup_accepted write FGroup_accepted;
    property group_invite: TArray<string> read FGroup_invite write FGroup_invite;
    property like: TArray<string> read FLike write FLike;
    property mention: TArray<string> read FMention write FMention;
    property msg: TArray<string> read FMsg write FMsg;
    property new_post: TArray<string> read FNew_post write FNew_post;
    property reply: TArray<string> read FReply write FReply;
    property repost: TArray<string> read FRepost write FRepost;
    property sdk_open: TArray<string> read FSdk_open write FSdk_open;
    property tag_photo: TArray<string> read FTag_photo write FTag_photo;
    property wall_post: TArray<string> read FWall_post write FWall_post;
    property wall_publish: TArray<string> read FWall_publish write FWall_publish;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPushSettingsItem;
  end;

  TVkPushSettings = class
  private
    FConversations: TVkConversations;
    FDisabled: Extended;
    Fsettings: TVkPushSettingsItem;
  public
    property conversations: TVkConversations read FConversations write FConversations;
    property disabled: Extended read FDisabled write FDisabled;
    property settings: TVkPushSettingsItem read Fsettings write Fsettings;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPushSettings;
  end;

implementation

{TVkConversation}

function TVkConversation.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkConversation.FromJsonString(AJsonString: string): TVkConversation;
begin
  result := TJson.JsonToObject<TVkConversation>(AJsonString)
end;

{TVkConversations}

destructor TVkConversations.Destroy;
var
  LitemsItem: TVkConversation;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkConversations.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkConversations.FromJsonString(AJsonString: string): TVkConversations;
begin
  result := TJson.JsonToObject<TVkConversations>(AJsonString)
end;

{TRootClass}

function TVkPushSettingsItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPushSettingsItem.FromJsonString(AJsonString: string): TVkPushSettingsItem;
begin
  result := TJson.JsonToObject<TVkPushSettingsItem>(AJsonString)
end;

{TVkPushSettings}

constructor TVkPushSettings.Create;
begin
  inherited;
  FConversations := TVkConversations.Create();
end;

destructor TVkPushSettings.Destroy;
begin
  FConversations.Free;
  inherited;
end;

function TVkPushSettings.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPushSettings.FromJsonString(AJsonString: string): TVkPushSettings;
begin
  result := TJson.JsonToObject<TVkPushSettings>(AJsonString)
end;

end.

