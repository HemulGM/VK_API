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
    property DisabledUntil: Extended read FDisabled_until write FDisabled_until;
    property PeerId: Extended read FPeer_id write FPeer_id;
    property Sound: Extended read FSound write FSound;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversation;
  end;

  TVkConversations = class
  private
    FCount: Extended;
    FItems: TArray<TVkConversation>;
  public
    property Count: Extended read FCount write FCount;
    property Items: TArray<TVkConversation> read FItems write FItems;
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
    property AppRequest: TArray<string> read FApp_request write FApp_request;
    property Birthday: TArray<string> read FBirthday write FBirthday;
    property Chat: TArray<string> read FChat write FChat;
    property Comment: TArray<string> read FComment write FComment;
    property EventSoon: TArray<string> read FEvent_soon write FEvent_soon;
    property Friend: TArray<string> read FFriend write FFriend;
    property FriendAccepted: TArray<string> read FFriend_accepted write FFriend_accepted;
    property FriendFound: TArray<string> read FFriend_found write FFriend_found;
    property GroupAccepted: TArray<string> read FGroup_accepted write FGroup_accepted;
    property GroupInvite: TArray<string> read FGroup_invite write FGroup_invite;
    property Like: TArray<string> read FLike write FLike;
    property Mention: TArray<string> read FMention write FMention;
    property Msg: TArray<string> read FMsg write FMsg;
    property NewPost: TArray<string> read FNew_post write FNew_post;
    property Reply: TArray<string> read FReply write FReply;
    property Repost: TArray<string> read FRepost write FRepost;
    property SDKOpen: TArray<string> read FSdk_open write FSdk_open;
    property TagPhoto: TArray<string> read FTag_photo write FTag_photo;
    property WallPost: TArray<string> read FWall_post write FWall_post;
    property WallPublish: TArray<string> read FWall_publish write FWall_publish;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPushSettingsItem;
  end;

  TVkPushSettings = class
  private
    FConversations: TVkConversations;
    FDisabled: Extended;
    Fsettings: TVkPushSettingsItem;
  public
    property Conversations: TVkConversations read FConversations write FConversations;
    property Disabled: Extended read FDisabled write FDisabled;
    property Settings: TVkPushSettingsItem read Fsettings write Fsettings;
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

