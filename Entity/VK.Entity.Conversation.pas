unit VK.Entity.Conversation;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Message, VK.Entity.Common, VK.Entity.User,
  VK.Entity.Group;

type
  TVkChatAccess = class
  private
    FCan_change_info: Boolean;
    FCan_change_invite_link: Boolean;
    FCan_change_pin: Boolean;
    FCan_copy_chat: Boolean;
    FCan_invite: Boolean;
    FCan_moderate: Boolean;
    FCan_promote_users: Boolean;
    FCan_see_invite_link: Boolean;
  public
    property CanChangeInfo: Boolean read FCan_change_info write FCan_change_info;
    property CanChangeInviteLink: Boolean read FCan_change_invite_link write FCan_change_invite_link;
    property CanChangePin: Boolean read FCan_change_pin write FCan_change_pin;
    property CanCopyChat: Boolean read FCan_copy_chat write FCan_copy_chat;
    property CanInvite: Boolean read FCan_invite write FCan_invite;
    property CanModerate: Boolean read FCan_moderate write FCan_moderate;
    property CanPromoteUsers: Boolean read FCan_promote_users write FCan_promote_users;
    property CanSeeInviteLink: Boolean read FCan_see_invite_link write FCan_see_invite_link;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatAccess;
  end;

  TVkChatSettings = class
  private
    FAcl: TVkChatAccess;
    FActive_ids: TArray<Extended>;
    FAdmin_ids: TArray<Extended>;
    FMembers_count: Extended;
    FOwner_id: Extended;
    FPhoto: TVkChatPhoto;
    FPinned_message: TVkMessage;
    FState: string;
    FTitle: string;
  public
    property ACL: TVkChatAccess read FAcl write FAcl;
    property ActiveIds: TArray<Extended> read FActive_ids write FActive_ids;
    property AdminIds: TArray<Extended> read FAdmin_ids write FAdmin_ids;
    property MembersCount: Extended read FMembers_count write FMembers_count;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
    property PinnedMessage: TVkMessage read FPinned_message write FPinned_message;
    property State: string read FState write FState;
    property Title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatSettings;
  end;

  TVkCanWrite = class
  private
    FAllowed: Boolean;
    FReason: Integer;
  public
    property Allowed: Boolean read FAllowed write FAllowed;
    property Reason: Integer read FReason write FReason;
    {
    18 Ч пользователь заблокирован или удален;
    900 Ч нельз€ отправить сообщение пользователю, который в чЄрном списке;
    901 Ч пользователь запретил сообщени€ от сообщества;
    902 Ч пользователь запретил присылать ему сообщени€ с помощью настроек приватности;
    915 Ч в сообществе отключены сообщени€;
    916 Ч в сообществе заблокированы сообщени€;
    917 Ч нет доступа к чату;
    918 Ч нет доступа к e-mail;
    203 Ч нет доступа к сообществу.
    }
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCanWrite;
  end;

  TVkPeer = class
  private
    FId: Integer;
    FLocal_id: Integer;
    FType: string;
  public
    property Id: Integer read FId write FId;
    property LocalId: Integer read FLocal_id write FLocal_id;
    property&Type: string read FType write FType;
    {user, chat}
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPeer;
  end;

  TVkConversation = class
  private
    FCan_write: TVkCanWrite;
    FChat_settings: TVkChatSettings;
    FIn_read: Integer;
    FLast_message_id: Integer;
    FOut_read: Integer;
    FPeer: TVkPeer;
    FUnread_count: Integer;
    FUnanswered: Boolean;
    FImportant: Boolean;
    FCan_send_money: Boolean;
    FCan_receive_money: Boolean;
    function GetIsChat: Boolean;
  public
    property CanWrite: TVkCanWrite read FCan_write write FCan_write;
    property ChatSettings: TVkChatSettings read FChat_settings write FChat_settings;
    property InRead: Integer read FIn_read write FIn_read;
    property LastMessageId: Integer read FLast_message_id write FLast_message_id;
    property OutRead: Integer read FOut_read write FOut_read;
    property Peer: TVkPeer read FPeer write FPeer;
    property UnreadCount: Integer read FUnread_count write FUnread_count;
    property Unanswered: Boolean read FUnanswered write FUnanswered;
    property Important: Boolean read FImportant write FImportant;
    property CanSendMoney: Boolean read FCan_send_money write FCan_send_money;
    property CanReceiveMoney: Boolean read FCan_receive_money write FCan_receive_money;
    property IsChat: Boolean read GetIsChat;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversation;
  end;

  TVkConversationItem = class
  private
    FConversation: TVkConversation;
    FLast_message: TVkMessage;
  public
    property Conversation: TVkConversation read FConversation write FConversation;
    property LastMessage: TVkMessage read FLast_message write FLast_message;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversationItem;
  end;

  TVkConversationItems = class
  private
    FCount: Integer;
    FItems: TArray<TVkConversationItem>;
    FProfiles: TArray<TVkUser>;
    FUnread_count: Integer;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property UnreadCount: Integer read FUnread_count write FUnread_count;
    property Items: TArray<TVkConversationItem> read FItems write FItems;
    property Profiles: TArray<TVkUser> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversationItems;
  end;

  TVkMessageHistory = class
  private
    FItems: TArray<TVkMessage>;
    FCount: Integer;
    FSaveObjects: Boolean;
    FConversations: TArray<TVkConversation>;
    FProfiles: TArray<TVkUser>;
    FSaveMessages: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
    procedure SetSaveMessages(const Value: Boolean);
  public
    property Items: TArray<TVkMessage> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property Conversations: TArray<TVkConversation> read FConversations write FConversations;
    property Profiles: TArray<TVkUser> read FProfiles write FProfiles;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    property SaveMessages: Boolean read FSaveMessages write SetSaveMessages;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageHistory;
  end;

implementation

{TVkChatAccess}

function TVkChatAccess.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChatAccess.FromJsonString(AJsonString: string): TVkChatAccess;
begin
  result := TJson.JsonToObject<TVkChatAccess>(AJsonString)
end;

{TVkChatSettings}

constructor TVkChatSettings.Create;
begin
  inherited;
  FAcl := TVkChatAccess.Create();
  FPhoto := TVkChatPhoto.Create();
end;

destructor TVkChatSettings.Destroy;
begin
  FAcl.Free;
  if Assigned(FPinned_message) then
    FPinned_message.Free;
  FPhoto.Free;
  inherited;
end;

function TVkChatSettings.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChatSettings.FromJsonString(AJsonString: string): TVkChatSettings;
begin
  result := TJson.JsonToObject<TVkChatSettings>(AJsonString)
end;

{TVkCanWrite}

function TVkCanWrite.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCanWrite.FromJsonString(AJsonString: string): TVkCanWrite;
begin
  result := TJson.JsonToObject<TVkCanWrite>(AJsonString)
end;

{TVkPeer}

function TVkPeer.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPeer.FromJsonString(AJsonString: string): TVkPeer;
begin
  result := TJson.JsonToObject<TVkPeer>(AJsonString)
end;

{TVkConversation}

constructor TVkConversation.Create;
begin
  inherited;
  FPeer := TVkPeer.Create();
  FCan_write := TVkCanWrite.Create();
end;

destructor TVkConversation.Destroy;
begin
  FPeer.Free;
  FCan_write.Free;
  if Assigned(FChat_settings) then
    FChat_settings.Free;
  inherited;
end;

function TVkConversation.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkConversation.FromJsonString(AJsonString: string): TVkConversation;
begin
  result := TJson.JsonToObject<TVkConversation>(AJsonString)
end;

function TVkConversation.GetIsChat: Boolean;
begin
  Result := Peer.&Type = 'chat';
end;

{ TVkConversationItem }

constructor TVkConversationItem.Create;
begin
  inherited;
end;

destructor TVkConversationItem.Destroy;
begin
  if Assigned(FConversation) then
    FConversation.Free;
  if Assigned(FLast_message) then
    FLast_message.Free;
  inherited;
end;

class function TVkConversationItem.FromJsonString(AJsonString: string): TVkConversationItem;
begin
  result := TJson.JsonToObject<TVkConversationItem>(AJsonString)
end;

function TVkConversationItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkConversationItems }

destructor TVkConversationItems.Destroy;
var
  LitemsItem: TVkConversationItem;
  LgroupItem: TVkGroup;
  LuserItem: TVkUser;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  for LgroupItem in FGroups do
    LgroupItem.Free;

  for LuserItem in FProfiles do
    LuserItem.Free;

  inherited;
end;

class function TVkConversationItems.FromJsonString(AJsonString: string): TVkConversationItems;
begin
  result := TJson.JsonToObject<TVkConversationItems>(AJsonString)
end;

function TVkConversationItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkMessageHistory }

constructor TVkMessageHistory.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkMessageHistory.Destroy;
var
  LItemsItem: TVkMessage;
  LuserItem: TVkUser;
  LconversationItem: TVkConversation;
begin
  if not FSaveObjects then
  begin
    if not FSaveMessages then
    begin
      for LItemsItem in FItems do
        LItemsItem.Free;
    end;

    for LuserItem in FProfiles do
      LuserItem.Free;

    for LconversationItem in FConversations do
      LconversationItem.Free;
  end;

  inherited;
end;

class function TVkMessageHistory.FromJsonString(AJsonString: string): TVkMessageHistory;
begin
  result := TJson.JsonToObject<TVkMessageHistory>(AJsonString);
end;

procedure TVkMessageHistory.SetSaveMessages(const Value: Boolean);
begin
  FSaveMessages := Value;
end;

procedure TVkMessageHistory.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

function TVkMessageHistory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

