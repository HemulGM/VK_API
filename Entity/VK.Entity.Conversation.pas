unit VK.Entity.Conversation;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Message, VK.Entity.Common, VK.Entity.Profile, VK.Entity.Group, VK.Types;

type
  TVkChatAccess = class(TVkEntity)
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
  end;

  TVkChatPermissions = class(TVkEntity)
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
  end;

  TVkChatSettings = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkCanWrite = class(TVkEntity)
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
  end;

  TVkPeer = class(TVkObject)
  private
    FLocal_id: Integer;
    FType: string;
    function GetType: TVkPeerType;
    procedure SetType(const Value: TVkPeerType);
    function GetIsChat: Boolean;
    function GetIsGroup: Boolean;
    function GetIsUser: Boolean;
  public
    property LocalId: Integer read FLocal_id write FLocal_id;
    property&Type: TVkPeerType read GetType write SetType;
    property IsUser: Boolean read GetIsUser;
    property IsGroup: Boolean read GetIsGroup;
    property IsChat: Boolean read GetIsChat;
  end;

  TVkConversationSort = class(TVkEntity)
  private
    FMinor_id: Integer;
    FMajor_id: Integer;
  public
    property MajorId: Integer read FMajor_id write FMajor_id;
    property MinorId: Integer read FMinor_id write FMinor_id;
  end;

  TVkConversation = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkConversations = class(TVkEntity)
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
  end;

  TVkConversationItem = class(TVkEntity)
  private
    FConversation: TVkConversation;
    FLast_message: TVkMessage;
  public
    property Conversation: TVkConversation read FConversation write FConversation;
    property LastMessage: TVkMessage read FLast_message write FLast_message;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkConversationItems = class(TVkEntity)
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
  end;

  TVkMessageHistory = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkImportantMessages = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

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

{ TVkConversationItems }

destructor TVkConversationItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversationItem>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
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

procedure TVkMessageHistory.SetSaveMessages(const Value: Boolean);
begin
  FSaveMessages := Value;
end;

procedure TVkMessageHistory.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkConversations }

destructor TVkConversations.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversation>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
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

end.

