﻿unit VK.Entity.Conversation;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Message, VK.Entity.Common,
  VK.Entity.Common.List, VK.Entity.Profile, VK.Entity.Group, VK.Types,
  VK.Entity.Common.ExtendedList, REST.JsonReflect, VK.Wrap.Interceptors,
  VK.Entity.Keyboard;

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
    FCan_change_style: Boolean;
  public
    property CanCall: Boolean read FCan_call write FCan_call;
    property CanChangeInfo: Boolean read FCan_change_info write FCan_change_info;
    property CanChangeInviteLink: Boolean read FCan_change_invite_link write FCan_change_invite_link;
    property CanChangePin: Boolean read FCan_change_pin write FCan_change_pin;
    property CanChangeStyle: Boolean read FCan_change_style write FCan_change_style;
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
    FChange_style: string;
  public
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property Call: string read FCall write FCall;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property ChangeAdmins: string read FChange_admins write FChange_admins;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property ChangeInfo: string read FChange_info write FChange_info;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property ChangePin: string read FChange_pin write FChange_pin;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property ChangeStyle: string read FChange_style write FChange_style;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property Invite: string read FInvite write FInvite;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property SeeInviteLink: string read FSee_invite_link write FSee_invite_link;
    /// <summary>
    /// owner_and_admins, all, owner,
    /// </summary>
    property UseMassMentions: string read FUse_mass_mentions write FUse_mass_mentions;
  end;

  TVkChatSettings = class(TVkEntity)
  private
    FAcl: TVkChatAccess;
    FActive_ids: TArray<TVkPeerId>;
    FAdmin_ids: TArray<TVkPeerId>;
    FMembers_count: Integer;
    FOwner_id: TVkPeerId;
    FPhoto: TVkChatPhoto;
    FPinned_message: TVkMessage;
    [JsonReflectAttribute(ctString, rtString, TChatStateInterceptor)]
    FState: TVkChatState;
    FTitle: string;
    FIs_group_channel: Boolean;
    FPermissions: TVkChatPermissions;
    FIs_disappearing: Boolean;
    FIs_service: Boolean;
    FFriends_count: Integer;
  public
    property ACL: TVkChatAccess read FAcl write FAcl;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property AdminIds: TArray<TVkPeerId> read FAdmin_ids write FAdmin_ids;
    property Permissions: TVkChatPermissions read FPermissions write FPermissions;
    property IsDisappearing: Boolean read FIs_disappearing write FIs_disappearing;
    property IsService: Boolean read FIs_service write FIs_service;
    /// <summary>
    /// Идентификаторы последних пользователей, писавших в чат
    /// </summary>
    property ActiveIds: TArray<TVkPeerId> read FActive_ids write FActive_ids;
    /// <summary>
    /// Число участников
    /// </summary>
    property MembersCount: Integer read FMembers_count write FMembers_count;
    /// <summary>
    /// Число друзей-участников
    /// </summary>
    property FriendsCount: Integer read FFriends_count write FFriends_count;
    /// <summary>
    /// Информация о том, является ли беседа каналом сообщества
    /// </summary>
    property IsGroupChannel: Boolean read FIs_group_channel write FIs_group_channel;
    /// <summary>
    /// Изображение-обложка чата
    /// </summary>
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Объект закреплённого сообщения, если есть
    /// </summary>
    property PinnedMessage: TVkMessage read FPinned_message write FPinned_message;
    /// <summary>
    ///  Статус текущего пользователя
    /// </summary>
    property State: TVkChatState read FState write FState;
    /// <summary>
    /// Название
    /// </summary>
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkCanWrite = class(TVkEntity)
  private
    FAllowed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TConversationDisableReasonInterceptor)]
    FReason: TVkConversationDisableReason;
  public
    property Allowed: Boolean read FAllowed write FAllowed;
    property Reason: TVkConversationDisableReason read FReason write FReason;
  end;

  TVkPeer = class(TVkEntity)
  private
    FLocal_id: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TPeerTypeInterceptor)]
    FType: TVkPeerType;
    FId: TVkPeerId;
    function GetIsChat: Boolean;
    function GetIsGroup: Boolean;
    function GetIsUser: Boolean;
  public
    /// <summary>
    /// Идентификатор назначения
    /// </summary>
    property Id: TVkPeerId read FId write FId;
    /// <summary>
    /// Локальный идентификатор назначения. Для чатов — id - 2000000000, для сообществ — -id, для e-mail — -(id+2000000000).
    /// </summary>
    property LocalId: TVkPeerId read FLocal_id write FLocal_id;
    /// <summary>
    /// Тип. Возможные значения: user, chat, group, email
    /// </summary>
    property &Type: TVkPeerType read FType write FType;
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

  TVkChatPushSettings = class(TVkEntity)
  private
    FDisabled_forever: Boolean;
    FNo_sound: Boolean;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDisabled_until: TDateTime;
    FDisabled_mass_mentions: Boolean;
    FDisabled_mentions: Boolean;
  public
    property DisabledUntil: TDateTime read FDisabled_until write FDisabled_until;
    property DisabledForever: Boolean read FDisabled_forever write FDisabled_forever;
    property NoSound: Boolean read FNo_sound write FNo_sound;
    property DisabledMentions: Boolean read FDisabled_mentions write FDisabled_mentions;
    property DisabledMassMentions: Boolean read FDisabled_mass_mentions write FDisabled_mass_mentions;
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
    FPush_settings: TVkChatPushSettings;
    FCurrent_keyboard: TVkKeyboard;
    FMentions: TVkPeerIds;
    FMention_cmids: TArrayOfInteger;
    FExpire_cmids: TArrayOfInteger;
  public
    FLast_conversation_message_id: Int64;
    //[JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_new: Boolean;
    FStyle: string;
    FIn_read_cmid: Integer;
    FOut_read_cmid: Integer;
    function GetIsChat: Boolean;
    function GetIsUser: Boolean;
    function GetIsGroup: Boolean;
  public
    property CanSendMoney: Boolean read FCan_send_money write FCan_send_money;
    property CanReceiveMoney: Boolean read FCan_receive_money write FCan_receive_money;
    /// <summary>
    /// Информация о том, может ли пользователь писать в диалог
    /// </summary>
    property CanWrite: TVkCanWrite read FCan_write write FCan_write;
    /// <summary>
    /// Настройки чата
    /// </summary>
    property ChatSettings: TVkChatSettings read FChat_settings write FChat_settings;
    property CurrentKeyboard: TVkKeyboard read FCurrent_keyboard write FCurrent_keyboard;
    /// <summary>
    /// True, если диалог помечен как важный (только для сообщений сообществ).
    /// </summary>
    property Important: Boolean read FImportant write FImportant;
    /// <summary>
    /// Идентификатор последнего прочтенного входящего сообщения.
    /// </summary>
    property InRead: Integer read FIn_read write FIn_read;
    /// <summary>
    /// Идентификатор последнего прочтенного входящего сообщения.
    /// </summary>
    property InReadCmid: Integer read FIn_read_cmid write FIn_read_cmid;
    property IsChat: Boolean read GetIsChat;
    property IsMarkedUnread: Boolean read FIs_marked_unread write FIs_marked_unread;
    property IsNew: Boolean read FIs_new write FIs_new;
    property IsUser: Boolean read GetIsUser;
    /// <summary>
    /// Идентификатор последнего сообщения
    /// </summary>
    property LastConversationMessageId: Int64 read FLast_conversation_message_id write FLast_conversation_message_id;
    /// <summary>
    /// Идентификатор последнего сообщения.
    /// </summary>
    property LastMessageId: Integer read FLast_message_id write FLast_message_id;
    /// <summary>
    /// Идентификатор последнего прочтенного исходящего сообщения.
    /// </summary>
    property OutRead: Integer read FOut_read write FOut_read;
        /// <summary>
    /// Идентификатор последнего прочтенного исходящего сообщения.
    /// </summary>
    property OutReadCmid: Integer read FOut_read_cmid write FOut_read_cmid;
    /// <summary>
    /// Информация о собеседнике
    /// </summary>
    property Peer: TVkPeer read FPeer write FPeer;
    /// <summary>
    /// Настройки Push-уведомлений
    /// </summary>
    property PushSettings: TVkChatPushSettings read FPush_settings write FPush_settings;
    property SortId: TVkConversationSort read FSort_id write FSort_id;
    property Style: string read FStyle write FStyle;
    /// <summary>
    /// Число непрочитанных сообщений.
    /// </summary>
    property UnreadCount: Integer read FUnread_count write FUnread_count;
    /// <summary>
    /// True, если диалог помечен как неотвеченный (только для сообщений сообществ).
    /// </summary>
    property Unanswered: Boolean read FUnanswered write FUnanswered;
    property Mentions: TVkPeerIds read FMentions write FMentions;
    property MentionCmids: TArrayOfInteger read FMention_cmids write FMention_cmids;
    property ExpireCmids: TArrayOfInteger read FExpire_cmids write FExpire_cmids;
    property IsGroup: Boolean read GetIsGroup;
    destructor Destroy; override;
  end;

  TVkConversations = TVkEntityExtendedList<TVkConversation>;

  TVkConversationItem = class(TVkEntity)
  private
    FConversation: TVkConversation;
    FLast_message: TVkMessage;
  public
    property Conversation: TVkConversation read FConversation write FConversation;
    property LastMessage: TVkMessage read FLast_message write FLast_message;
    destructor Destroy; override;
  end;

  TVkConversationItems = class(TVkEntityExtendedList<TVkConversationItem>)
  private
    FUnread_count: Integer;
  public
    property UnreadCount: Integer read FUnread_count write FUnread_count;
  end;

  TVkMessageHistory = class(TVkEntityExtendedList<TVkMessage>)
  private
    FConversations: TArray<TVkConversation>;
    FSaveMessages: Boolean;
    procedure SetSaveMessages(const Value: Boolean);
  public
    property Conversations: TArray<TVkConversation> read FConversations write FConversations;
    //
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
    destructor Destroy; override;
  end;

  TVkConversationMember = class(TVkEntity)
  private
    FInvited_By: TVkPeerId;
    FIs_Admin: Boolean;
    FIs_Owner: Boolean;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FJoin_Date: TDateTime;
    FMember_Id: TVkPeerid;
  public
    property InvitedBy: TVkPeerId read FInvited_By write FInvited_By;
    property IsAdmin: Boolean read FIs_Admin write FIs_Admin;
    property IsOwner: Boolean read FIs_Owner write FIs_Owner;
    property JoinDate: TDateTime read FJoin_Date write FJoin_Date;
    property MemberId: TVkPeerId read FMember_Id write FMember_Id;
  end;

  TVkConversationMembers = TVkEntityExtendedList<TVkConversationMember>;

implementation

uses
  VK.CommonUtils;

{TVkChatSettings}

destructor TVkChatSettings.Destroy;
begin
  if Assigned(FAcl) then
    FAcl.Free;
  if Assigned(FPinned_message) then
    FPinned_message.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FPermissions) then
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

{TVkConversation}

destructor TVkConversation.Destroy;
begin
  if Assigned(FPeer) then
    FPeer.Free;
  if Assigned(FSort_id) then
    FSort_id.Free;
  if Assigned(FCan_write) then
    FCan_write.Free;
  if Assigned(FChat_settings) then
    FChat_settings.Free;
  if Assigned(FPush_settings) then
    FPush_settings.Free;
  if Assigned(FCurrent_keyboard) then
    FCurrent_keyboard.Free;
  inherited;
end;

function TVkConversation.GetIsGroup: Boolean;
begin
  Result := Peer.&Type = TVkPeerType.Group;
end;

function TVkConversation.GetIsChat: Boolean;
begin
  Result := Peer.&Type = TVkPeerType.Chat;
end;

function TVkConversation.GetIsUser: Boolean;
begin
  Result := Peer.&Type = TVkPeerType.User;
end;

{ TVkConversationItem }

destructor TVkConversationItem.Destroy;
begin
  if Assigned(FConversation) then
    FConversation.Free;
  if Assigned(FLast_message) then
    FLast_message.Free;
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
      TArrayHelp.FreeArrayOfObject<TVkMessage>(FItems);

    TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
    TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
    TArrayHelp.FreeArrayOfObject<TVkConversation>(FConversations);
  end;
end;

procedure TVkMessageHistory.SetSaveMessages(const Value: Boolean);
begin
  FSaveMessages := Value;
end;

{ TVkImportantMessages }

destructor TVkImportantMessages.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkConversation>(FConversations);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  if Assigned(FMessages) then
    FMessages.Free;
  inherited;
end;

end.

