unit ChatFMX.View.ChatItem;

interface

uses
  System.SysUtils, FMX.ListBox, VK.API, System.Messaging, System.Classes,
  FMX.Objects, System.UITypes, VK.Types, System.Types, FMX.StdCtrls, FMX.Types,
  VK.Entity.Message, VK.Entity.Common.ExtendedList, VK.Entity.Conversation;

type
  TListBoxItemChat = class(TListBoxItem)
  private
    FImageUrl: string;
    FImageFile: string;
    FUndreadCount: Integer;
    FLastTime: TDateTime;
    FIsMuted: Boolean;
    FVK: TCustomVK;
    FWasQueryImage: Boolean;
    FUnanswered: Boolean;
    FFromText: string;
    FConversationId: TVkPeerId;
    FIsOnline: Boolean;
    FIsOnlineMobile: Boolean;
    FIsPinned: Boolean;
    FIsSelfChat: Boolean;
    FIsHaveMention: Boolean;
    FVerified: Boolean;
    FSortMinorId: Int64;
    FSortMajorId: Int64;
    FOnChangeSortId: TNotifyEvent;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnUserStatusChange(const Sender: TObject; const M: TMessage);
    procedure SetUndreadCount(const Value: Integer);
    procedure SetLastTime(const Value: TDateTime);
    procedure SetIsMuted(const Value: Boolean);
    procedure SetUnanswered(const Value: Boolean);
    procedure SetFromText(const Value: string);
    procedure SetMessageText(const Value: string; IsAttach: Boolean);
    procedure SetConversationId(const Value: TVkPeerId);
    procedure SetIsOnline(const Value: Boolean);
    procedure SetIsOnlineMobile(const Value: Boolean);
    procedure SetIsPinned(const Value: Boolean);
    procedure SetIsSelfChat(const Value: Boolean);
    procedure SetIsHaveMention(const Value: Boolean);
    procedure SetVerified(const Value: Boolean);
    procedure FOnChangeMessage(const Sender: TObject; const M: TMessage);
    procedure UpdatePinned;
    procedure UpdateInfo;
    procedure SetSortMajorId(const Value: Int64);
    procedure SetSortMinorId(const Value: Int64);
    procedure Update(LastMessageId: Int64);
    procedure FOnUpdateChat(const Sender: TObject; const M: TMessage);
    procedure FOnChangeSort(const Sender: TObject; const M: TMessage);
    procedure SetOnChangeSortId(const Value: TNotifyEvent);
  protected
    procedure SetText(const Value: string); override;
    procedure DoChangeSortId;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK);
    procedure Fill(Item: TVkConversationItem; Data: IExtended);
    procedure ApplyStyle; override;
    property IsMuted: Boolean read FIsMuted write SetIsMuted;
    property UnreadCount: Integer read FUndreadCount write SetUndreadCount;
    property LastTime: TDateTime read FLastTime write SetLastTime;
    property Unanswered: Boolean read FUnanswered write SetUnanswered;
    property FromText: string read FFromText write SetFromText;
    property ConversationId: TVkPeerId read FConversationId write SetConversationId;
    property IsOnline: Boolean read FIsOnline write SetIsOnline;
    property IsOnlineMobile: Boolean read FIsOnlineMobile write SetIsOnlineMobile;
    property IsPinned: Boolean read FIsPinned write SetIsPinned;
    property IsSelfChat: Boolean read FIsSelfChat write SetIsSelfChat;
    property IsHaveMention: Boolean read FIsHaveMention write SetIsHaveMention;
    property Verified: Boolean read FVerified write SetVerified;
    procedure UpdateLastMessage(LastMessage: TVkMessage; Data: IExtended);
    procedure UpdateConversation; overload;
    procedure UpdateConversation(Conversation: TVkConversation; Data: IExtended); overload;
    property SortMajorId: Int64 read FSortMajorId write SetSortMajorId;
    property SortMinorId: Int64 read FSortMinorId write SetSortMinorId;
    property OnChangeSortId: TNotifyEvent read FOnChangeSortId write SetOnChangeSortId;
    destructor Destroy; override;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading, System.DateUtils, FMX.Graphics,
  VK.Entity.Group, VK.Entity.Profile, ChatFMX.Utils, ChatFMX.Events, VK.Messages;

{ TListBoxItemChat }

procedure TListBoxItemChat.ApplyStyle;
begin
  inherited;
  DisableDisappear := True;
  if not FWasQueryImage then
  begin
    FWasQueryImage := True;
    if not FImageUrl.IsEmpty then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end;
  var Circle: TCircle;
  if FindStyleResource('avatar', Circle) then
    if IsSelfChat then
    begin
      try
        var RS: TResourceStream := TResourceStream.Create(HInstance, 'im_favorites_100', RT_RCDATA);
        try
          Circle.Fill.Bitmap.Bitmap.LoadFromStream(RS);
        finally
          RS.Free;
        end;
      except
        Circle.Fill.Kind := TBrushKind.Solid;
        Exit;
      end;
      Circle.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      Circle.Fill.Kind := TBrushKind.Bitmap;
    end
    else
    begin
      if not FImageFile.IsEmpty then
      begin
        try
          Circle.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
        except
          FImageFile := '';
          Circle.Fill.Kind := TBrushKind.Solid;
          Exit;
        end;
        Circle.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
        Circle.Fill.Kind := TBrushKind.Bitmap;
      end
      else
        Circle.Fill.Kind := TBrushKind.Solid;
    end;
end;

constructor TListBoxItemChat.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  Cursor := crHandPoint;
  FWasQueryImage := False;
  FVK := AVK;
  HitTest := True;
  StyleLookup := 'item_chat';
  UnreadCount := 0;
  IsMuted := False;
  IsSelfChat := False;
  FromText := '';
  TextSettings.WordWrap := False;
  TextSettings.Trimming := TTextTrimming.Character;

  Event.Subscribe(TEventUserStatus, FOnUserStatusChange);
  Event.Subscribe(TEventNewMessage, FOnChangeMessage);
  Event.Subscribe(TEventEditMessage, FOnChangeMessage);
  Event.Subscribe(TEventDeleteMessage, FOnUpdateChat);
  Event.Subscribe(TEventChangeSort, FOnChangeSort);
end;

destructor TListBoxItemChat.Destroy;
begin
  Event.Unsubscribe(TEventUserStatus, FOnUserStatusChange);
  Event.Unsubscribe(TEventNewMessage, FOnChangeMessage);
  Event.Unsubscribe(TEventEditMessage, FOnChangeMessage);
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TListBoxItemChat.DoChangeSortId;
begin
  if Assigned(FOnChangeSortId) then
    FOnChangeSortId(Self);
end;

procedure TListBoxItemChat.Fill(Item: TVkConversationItem; Data: IExtended);
begin
  IsOnline := False;
  IsOnlineMobile := False;
  Verified := False;
  SetMessageText('', False);

  UpdateConversation(Item.Conversation, Data);
  if Assigned(Item.LastMessage) then
    UpdateLastMessage(Item.LastMessage, Data);
end;

procedure TListBoxItemChat.FOnChangeMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventNewMessage absolute M;
begin
  if Event.Data.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  Update(Event.Data.MessageId);
end;

procedure TListBoxItemChat.FOnChangeSort(const Sender: TObject; const M: TMessage);
var
  Event: TEventChangeSort absolute M;
begin
  if Event.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  case Event.IsMajor of
    True:
      SortMajorId := Event.NewId;
    False:
      SortMinorId := Event.NewId;
  end;
  DoChangeSortId;
end;

procedure TListBoxItemChat.FOnUpdateChat(const Sender: TObject; const M: TMessage);
var
  Event: TEventNewMessage absolute M;
begin
  if Event.Data.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  UpdateConversation;
end;

procedure TListBoxItemChat.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  NeedStyleLookup;
end;

procedure TListBoxItemChat.FOnUserStatusChange(const Sender: TObject; const M: TMessage);
var
  Event: TEventUserStatus absolute M;
begin
  if Event.UserId = FConversationId then
  begin
    IsOnline := Event.IsOnline;
    Self.IsOnlineMobile := IsOnline and Event.VkPlatform.IsMobile;
  end;
end;

procedure TListBoxItemChat.SetConversationId(const Value: TVkPeerId);
begin
  FConversationId := Value;
end;

procedure TListBoxItemChat.SetFromText(const Value: string);
begin
  FFromText := Value;
  StylesData['from'] := FFromText;
end;

procedure TListBoxItemChat.SetIsHaveMention(const Value: Boolean);
begin
  FIsHaveMention := Value;
  StylesData['mention_layout.Visible'] := FIsHaveMention;
  UpdatePinned;
  UpdateInfo;
end;

procedure TListBoxItemChat.SetIsMuted(const Value: Boolean);
begin
  FIsMuted := Value;
  if FIsMuted then
    StylesData['count_circ.Fill.Color'] := $FF828282
  else
    StylesData['count_circ.Fill.Color'] := TAlphaColorRec.White;
  StylesData['mute.Visible'] := FIsMuted;
end;

procedure TListBoxItemChat.SetIsOnline(const Value: Boolean);
begin
  FIsOnline := Value;
  StylesData['online.Visible'] := FIsOnline and (not FIsOnlineMobile);
  StylesData['online_mobile.Visible'] := FIsOnline and FIsOnlineMobile;
end;

procedure TListBoxItemChat.SetIsOnlineMobile(const Value: Boolean);
begin
  FIsOnlineMobile := Value;
  StylesData['online.Visible'] := FIsOnline and (not FIsOnlineMobile);
  StylesData['online_mobile.Visible'] := FIsOnline and FIsOnlineMobile;
end;

procedure TListBoxItemChat.UpdatePinned;
begin
  StylesData['pinned_layout.Visible'] := FIsPinned and (FUndreadCount = 0);
end;

procedure TListBoxItemChat.SetIsPinned(const Value: Boolean);
begin
  FIsPinned := Value;
  UpdatePinned;
end;

procedure TListBoxItemChat.SetIsSelfChat(const Value: Boolean);
begin
  FIsSelfChat := Value;
end;

procedure TListBoxItemChat.SetLastTime(const Value: TDateTime);
begin
  FLastTime := Value;
  StylesData['time.Visible'] := Value > 0;
  if IsToday(Value) then
    StylesData['time'] := FormatDateTime('HH:nn', Value)
  else if YearOf(Value) = YearOf(Now) then
    StylesData['time'] := FormatDateTime('D mmm', Value)
  else
    StylesData['time'] := FormatDateTime('D mmm YYYY', Value);
end;

procedure TListBoxItemChat.SetMessageText(const Value: string; IsAttach: Boolean);
begin
  ItemData.Detail := ParseMention(PrepareForPreview(Value));
  if not IsSelected then
    if IsAttach then
      StylesData['detail.TextSettings.FontColor'] := $FF71AAEB
    else
      StylesData['detail.TextSettings.FontColor'] := $FF898989;
  StylesData['detail_sel.StartValue'] := StylesData['detail.TextSettings.FontColor'].AsInteger;
  if ItemData.Detail.IsEmpty then
    ItemData.Detail := 'Пустое сообщение';
end;

procedure TListBoxItemChat.SetOnChangeSortId(const Value: TNotifyEvent);
begin
  FOnChangeSortId := Value;
end;

procedure TListBoxItemChat.SetSortMajorId(const Value: Int64);
begin
  FSortMajorId := Value;
end;

procedure TListBoxItemChat.SetSortMinorId(const Value: Int64);
begin
  FSortMinorId := Value;
end;

procedure TListBoxItemChat.SetText(const Value: string);
begin
  inherited;
end;

procedure TListBoxItemChat.SetUnanswered(const Value: Boolean);
begin
  FUnanswered := Value;
  StylesData['unread.Visible'] := Value;
  UpdatePinned;
  UpdateInfo;
end;

procedure TListBoxItemChat.UpdateInfo;
begin
  StylesData['info_bottom.Visible'] :=
    StylesData['count_layout.Visible'].AsBoolean or
    StylesData['unread.Visible'].AsBoolean or
    StylesData['mention_layout.Visible'].AsBoolean or
    StylesData['pinned_layout.Visible'].AsBoolean;
end;

procedure TListBoxItemChat.SetUndreadCount(const Value: Integer);
begin
  FUndreadCount := Value;
  StylesData['count_layout.Visible'] := Value > 0;
  StylesData['count'] := Value.ToString;
  UpdatePinned;
  UpdateInfo;
end;

procedure TListBoxItemChat.SetVerified(const Value: Boolean);
begin
  FVerified := Value;
  StylesData['verified.Visible'] := FVerified;
end;

procedure TListBoxItemChat.UpdateConversation;
begin
  TTask.Run(
    procedure
    var
      Params: TVkParamsConversationsGetById;
      Items: TVkConversations;
    begin
      Params.PeerId(FConversationId);
      Params.Extended;
      Params.Fields(
        [TVkExtendedField.OnlineInfo, TVkExtendedField.FirstNameAcc,
        TVkExtendedField.IsFriend, TVkExtendedField.CanSendFriendRequest,
        TVkExtendedField.CanWritePrivateMessage,
        TVkExtendedField.Verified, TVkExtendedField.Photo50]);
      if FVK.Messages.GetConversationsById(Items, Params) then
      begin
        var Extended: IExtended := Items;
        if Length(Items.Items) > 0 then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              UpdateConversation(Items.Items[0], Extended);
            end);
        end;
      end;
    end);
end;

procedure TListBoxItemChat.Update(LastMessageId: Int64);
begin
  TTask.Run(
    procedure
    var
      Params: TVkParamsMessageHistory;
      Items: TVkMessageHistory;
    begin
      Params.PeerId(FConversationId);
      Params.Count(1);
      Params.Extended;
      Params.Fields(
        [TVkExtendedField.OnlineInfo, TVkExtendedField.FirstNameAcc,
        TVkExtendedField.IsFriend, TVkExtendedField.CanSendFriendRequest,
        TVkExtendedField.CanWritePrivateMessage,
        TVkExtendedField.Verified, TVkExtendedField.Photo50]);
      if FVK.Messages.GetHistory(Items, Params) then
      begin
        var Extended: IExtended := Items;
        if Length(Items.Items) > 0 then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              UpdateLastMessage(Items.Items[0], Extended);
            end);
        end;
      end;
    end);
end;

procedure TListBoxItemChat.UpdateConversation(Conversation: TVkConversation; Data: IExtended);
begin
  SortMajorId := Conversation.SortId.MajorId;
  SortMinorId := Conversation.SortId.MinorId;

  IsHaveMention := not Conversation.Mentions.IsEmpty;
  IsSelfChat := Conversation.Peer.Id = FVK.UserId;
  FConversationId := Conversation.Peer.Id;
  UnreadCount := Conversation.UnreadCount;
  IsPinned := SortMajorId <> 0;

  if Assigned(Conversation.PushSettings) then
  begin
    IsMuted := Conversation.PushSettings.NoSound or Conversation.PushSettings.DisabledForever;
  end;

  Unanswered := (Conversation.UnreadCount = 0) and (Conversation.InRead <> Conversation.OutRead);

  if IsSelfChat then
  begin
    Text := 'Избранное';
    Exit;
  end;
  if Conversation.IsChat then
  begin
    if Assigned(Conversation.ChatSettings) then
    begin
      Text := Conversation.ChatSettings.Title;
      if Assigned(Conversation.ChatSettings.Photo) then
        FImageUrl := Conversation.ChatSettings.Photo.Photo50;
    end;
  end
  else if Conversation.IsUser then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Conversation.Peer.Id, User) then
    begin
      Text := User.FullName;
      Verified := User.Verified;
      FImageUrl := User.Photo50;

      if Assigned(User.OnlineInfo) then
      begin
        IsOnline := User.OnlineInfo.IsOnline;
        IsOnlineMobile := User.OnlineInfo.IsMobile;
      end;
    end;
  end
  else if Conversation.IsGroup then
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Conversation.Peer.Id, Group) then
    begin
      Text := Group.Name;
      FImageUrl := Group.Photo50;
      Verified := Group.Verified;
    end;
  end;
end;

procedure TListBoxItemChat.UpdateLastMessage;
begin
  LastTime := LastMessage.Date;

  if LastMessage.FromId = FVK.UserId then
  begin
    if not IsSelfChat then
      FromText := 'Вы: ';
  end
  else
  begin
    if FConversationId <> LastMessage.FromId then
    begin
      if PeerIdIsUser(LastMessage.FromId) then
      begin
        var User: TVkProfile;
        if Data.GetProfileById(LastMessage.FromId, User) then
          FromText := User.FirstName + ': ';
      end
      else
      begin
        var Group: TVkGroup;
        if Data.GetGroupById(LastMessage.FromId, Group) then
          FromText := Group.Name + ': ';
      end;
    end;
  end;
      // Текст последнего сообщения
  if not LastMessage.Text.IsEmpty then
    SetMessageText(LastMessage.Text, False)
      // Вложение
  else if Length(LastMessage.Attachments) > 0 then
  begin
    var AttachText := AttachmentToText(LastMessage.Attachments[0].&Type);
    SetMessageText(AttachText, True);
  end
      // Действие
  else if Assigned(LastMessage.Action) then
  begin
    FromText := '';
    var MemberText := '';

    if PeerIdIsUser(LastMessage.Action.MemberId) then
    begin
      var User: TVkProfile;
      if Data.GetProfileById(LastMessage.Action.MemberId, User) then
        if User.FirstNameAcc.IsEmpty then
          MemberText := User.FullName
        else
          MemberText := User.FullNameAcc;
    end
    else
    begin
      var Group: TVkGroup;
      if Data.GetGroupById(LastMessage.Action.MemberId, Group) then
        MemberText := Group.Name;
    end;

    var ActionFromText := '';
    if PeerIdIsUser(LastMessage.FromId) then
    begin
      var User: TVkProfile;
      if Data.GetProfileById(LastMessage.FromId, User) then
        ActionFromText := User.FullName;
    end
    else
    begin
      var Group: TVkGroup;
      if Data.GetGroupById(LastMessage.FromId, Group) then
        ActionFromText := Group.Name;
    end;

    var ActionText := MessageActionToText(LastMessage.Action, LastMessage.FromId, ActionFromText, MemberText);
    SetMessageText(ActionText, False);
  end
      // Пересланные сообщения
  else if Length(LastMessage.FwdMessages) > 0 then
  begin
    SetMessageText(Length(LastMessage.FwdMessages).ToString + WordOfCount(Length(LastMessage.FwdMessages), [' сообщение', ' сообщения', ' сообщений']), True);
  end
  else if Assigned(LastMessage.Geo) then
  begin
    SetMessageText('Карта', True)
  end;
end;

end.

