unit VK.Entity.PushSettings;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkConversation = class(TVkEntity)
  private
    FDisabled_until: Int64;
    FPeer_id: Integer;
    FSound: Boolean;
  public
    property DisabledUntil: Int64 read FDisabled_until write FDisabled_until;
    property PeerId: Integer read FPeer_id write FPeer_id;
    property Sound: Boolean read FSound write FSound;
  end;

  TVkConversations = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkConversation>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkConversation> read FItems write FItems;
    destructor Destroy; override;
  end;

  TVkPushSettingsItem = class(TVkEntity)
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
  end;

  TVkPushSettings = class(TVkEntity)
  private
    FConversations: TVkConversations;
    FDisabled: Extended;
    Fsettings: TVkPushSettingsItem;
  public
    property Conversations: TVkConversations read FConversations write FConversations;
    property Disabled: Extended read FDisabled write FDisabled;
    property Settings: TVkPushSettingsItem read Fsettings write Fsettings;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkConversations}

destructor TVkConversations.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversation>(FItems);
  inherited;
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

end.

