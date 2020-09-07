unit VK.Entity.Conversation;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Message, VK.Entity.Common, VK.Entity.Profile, VK.Entity.Group, VK.Types;

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
    FCan_call: Boolean;
    FCan_use_mass_mentions: Boolean;
    FCan_change_service_type: Boolean;
  public
    property CanCall: Boolean read FCan_call write FCan_call;
    property CanChangeInfo: Boolean read FCan_change_info write FCan_change_info;
    property CanChangeInviteLink: Boolean read FCan_change_invite_link write FCan_change_invite_link;
    property CanChangePin: Boolean read FCan_change_pin write FCan_change_pin;
    property CanChangeServiceType: Boolean read FCan_change_service_type write FCan_change_service_type;
    property CanCopyChat: Boolean read FCan_copy_chat write FCan_copy_chat;
    property CanInvite: Boolean read FCan_invite write FCan_invite;
    property CanModerate: Boolean read FCan_moderate write FCan_moderate;
    property CanPromoteUsers: Boolean read FCan_promote_users write FCan_promote_users;
    property CanSeeInviteLink: Boolean read FCan_see_invite_link write FCan_see_invite_link;
    property CanUseMassMentions: Boolean read FCan_use_mass_mentions write FCan_use_mass_mentions;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatAccess;
  end;

  TVkChatPermissions = class
  private
    FCall: string;
    FChange_admins: string;
    FChange_info: string;
    FChange_pin: string;
    FInvite: string;
    FSee_invite_link: string;
    FUse_mass_mentions: string;
  public
    property Call: string read FCall write FCall;
    property ChangeAdmins: string read FChange_admins write FChange_admins;
    property ChangeInfo: string read FChange_info write FChange_info;
    property ChangePin: string read FChange_pin write FChange_pin;
    property Invite: string read FInvite write FInvite;
    property SeeInviteLink: string read FSee_invite_link write FSee_invite_link;
    property UseMassMentions: string read FUse_mass_mentions write FUse_mass_mentions;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPermissions;
  end;

  TVkChatSettings = class
  private
    FAcl: TVkChatAccess;
    FActive_ids: TArray<Integer>;
    FAdmin_ids: TArray<Integer>;
    FMembers_count: Integer;
    FOwner_id: Integer;
    FPhoto: TVkChatPhoto;
    FPinned_message: TVkMessage;
    FState: string;
    FTitle: string;
    FIs_group_channel: Boolean;
    FPermissions: TVkChatPermissions;
    FIs_disappearing: Boolean;
    FIs_service: Boolean;
  public
    property ACL: TVkChatAccess read FAcl write FAcl;
    property ActiveIds: TArray<Integer> read FActive_ids write FActive_ids;
    property AdminIds: TArray<Integer> read FAdmin_ids write FAdmin_ids;
    property MembersCount: Integer read FMembers_count write FMembers_count;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property IsGroupChannel: Boolean read FIs_group_channel write FIs_group_channel;
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
    property PinnedMessage: TVkMessage read FPinned_message write FPinned_message;
    property State: string read FState write FState;
    property Title: string read FTitle write FTitle;
    property Permissions: TVkChatPermissions read FPermissions write FPermissions;
    property IsDisappearing: Boolean read FIs_disappearing write FIs_disappearing;
    property IsService: Boolean read FIs_service write FIs_service;
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
    function GetType: TVkPeerType;
    procedure SetType(const Value: TVkPeerType);
    function GetIsChat: Boolean;
    function GetIsGroup: Boolean;
    function GetIsUser: Boolean;
  public
    property Id: Integer read FId write FId;
    property LocalId: Integer read FLocal_id write FLocal_id;
    property&Type: TVkPeerType read GetType write SetType;
    property IsUser: Boolean read GetIsUser;
    property IsGroup: Boolean read GetIsGroup;
    property IsChat: Boolean read GetIsChat;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPeer;
  end;

  TVkConversationSort = class
  private
    FMinor_id: Integer;
    FMajor_id: Integer;
  public
    property MajorId: Integer read FMajor_id write FMajor_id;
    property MinorId: Integer read FMinor_id write FMinor_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversationSort;
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
    FSort_id: TVkConversationSort;
    FIs_marked_unread: Boolean;
    function GetIsChat: Boolean;
    function GetIsUser: Boolean;
  public
    property CanWrite: TVkCanWrite read FCan_write write FCan_write;
    property ChatSettings: TVkChatSettings read FChat_settings write FChat_settings;
    property InRead: Integer read FIn_read write FIn_read;
    property LastMessageId: Integer read FLast_message_id write FLast_message_id;
    property OutRead: Integer read FOut_read write FOut_read;
    property Peer: TVkPeer read FPeer write FPeer;
    property SortId: TVkConversationSort read FSort_id write FSort_id;
    property IsMarkedUnread: Boolean read FIs_marked_unread write FIs_marked_unread;
    property UnreadCount: Integer read FUnread_count write FUnread_count;
    property Unanswered: Boolean read FUnanswered write FUnanswered;
    property Important: Boolean read FImportant write FImportant;
    property CanSendMoney: Boolean read FCan_send_money write FCan_send_money;
    property CanReceiveMoney: Boolean read FCan_receive_money write FCan_receive_money;
    property IsChat: Boolean read GetIsChat;
    property IsUser: Boolean read GetIsUser;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversation;
  end;

  TVkConversations = class
  private
    FCount: Integer;
    FItems: TArray<TVkConversation>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkConversation> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversations;
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
    FProfiles: TArray<TVkProfile>;
    FUnread_count: Integer;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property UnreadCount: Integer read FUnread_count write FUnread_count;
    property Items: TArray<TVkConversationItem> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
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
    FProfiles: TArray<TVkProfile>;
    FSaveMessages: Boolean;
    FGroups: TArray<TVkGroup>;
    procedure SetSaveObjects(const Value: Boolean);
    procedure SetSaveMessages(const Value: Boolean);
  public
    property Items: TArray<TVkMessage> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property Conversations: TArray<TVkConversation> read FConversations write FConversations;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    //
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    property SaveMessages: Boolean read FSaveMessages write SetSaveMessages;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageHistory;
  end;

  TVkImportantMessages = class
  private
    FMessages: TVkMessages;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FConversations: TArray<TVkConversation>;
  public
    property Messages: TVkMessages read FMessages write FMessages;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Conversations: TArray<TVkConversation> read FConversations write FConversations;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkImportantMessages;
  end;

implementation

uses
  VK.CommonUtils;

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
  FPermissions := TVkChatPermissions.Create;
end;

destructor TVkChatSettings.Destroy;
begin
  FAcl.Free;
  if Assigned(FPinned_message) then
    FPinned_message.Free;
  FPhoto.Free;
  FPermissions.Free;
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

function TVkPeer.GetIsChat: Boolean;
begin
  Result := PeerIdIsChat(FId);
end;

function TVkPeer.GetIsGroup: Boolean;
begin
  Result := PeerIdIsGroup(FId);
end;

function TVkPeer.GetIsUser: Boolean;
begin
  Result := PeerIdIsUser(FId);
end;

function TVkPeer.GetType: TVkPeerType;
begin
  Result := TVkPeerType.Create(FType);
end;

procedure TVkPeer.SetType(const Value: TVkPeerType);
begin
  FType := Value.ToString;
end;

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
  FSort_id := TVkConversationSort.Create;
  FCan_write := TVkCanWrite.Create();
end;

destructor TVkConversation.Destroy;
begin
  FPeer.Free;
  FSort_id.Free;
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
  Result := Peer.&Type = ptChat;
end;

function TVkConversation.GetIsUser: Boolean;
begin
  Result := Peer.&Type = ptUser;
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
begin
  TArrayHelp.FreeArrayOfObject<TVkConversationItem>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
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
begin
  if not FSaveObjects then
  begin
    if not FSaveMessages then
    begin
      TArrayHelp.FreeArrayOfObject<TVkMessage>(FItems);
    end;

    TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
    TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
    TArrayHelp.FreeArrayOfObject<TVkConversation>(FConversations);
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

{ TVkConversationSort }

class function TVkConversationSort.FromJsonString(AJsonString: string): TVkConversationSort;
begin
  result := TJson.JsonToObject<TVkConversationSort>(AJsonString);
end;

function TVkConversationSort.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkChatPermissions }

class function TVkChatPermissions.FromJsonString(AJsonString: string): TVkChatPermissions;
begin
  result := TJson.JsonToObject<TVkChatPermissions>(AJsonString);
end;

function TVkChatPermissions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkConversations }

destructor TVkConversations.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversation>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
end;

class function TVkConversations.FromJsonString(AJsonString: string): TVkConversations;
begin
  result := TJson.JsonToObject<TVkConversations>(AJsonString);
end;

function TVkConversations.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkImportantMessages }

constructor TVkImportantMessages.Create;
begin
  FMessages := TVkMessages.Create;
end;

destructor TVkImportantMessages.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversation>(FConversations);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  FMessages.Free;
  inherited;
end;

class function TVkImportantMessages.FromJsonString(AJsonString: string): TVkImportantMessages;
begin
  result := TJson.JsonToObject<TVkImportantMessages>(AJsonString);
end;

function TVkImportantMessages.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

