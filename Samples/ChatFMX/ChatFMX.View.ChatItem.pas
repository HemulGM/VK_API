unit ChatFMX.View.ChatItem;

interface

uses
  System.SysUtils, FMX.ListBox, VK.API, VK.Entity.Conversation, System.Messaging,
  System.Classes, FMX.Objects, System.UITypes, VK.Types, System.Types,
  FMX.StdCtrls, FMX.Types;

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
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
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
  protected
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK);
    procedure Fill(Item: TVkConversationItem; Data: TVkConversationItems);
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
    destructor Destroy; override;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.DateUtils, FMX.Graphics, VK.Entity.Group,
  VK.Entity.Profile, ChatFMX.Utils;

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
end;

destructor TListBoxItemChat.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TListBoxItemChat.Fill(Item: TVkConversationItem; Data: TVkConversationItems);
begin
  IsOnline := False;
  IsOnlineMobile := False;
  Verified := False;
  SetMessageText('', False);

  IsHaveMention := not Item.Conversation.Mentions.IsEmpty;
  IsSelfChat := Item.Conversation.Peer.Id = FVK.UserId;
  FConversationId := Item.Conversation.Peer.Id;
  UnreadCount := Item.Conversation.UnreadCount;
  //IsPinned := Item.Conversation.ChatSettings

  if Assigned(Item.LastMessage) then
  begin
    LastTime := Item.LastMessage.Date;
    if Item.LastMessage.FromId = FVK.UserId then
    begin
      if not IsSelfChat then
        FromText := 'Вы: ';
    end
    else
    begin
      if Item.Conversation.Peer.Id <> Item.LastMessage.FromId then
      begin
        if PeerIdIsUser(Item.LastMessage.FromId) then
        begin
          var User: TVkProfile;
          if Data.GetProfileById(Item.LastMessage.FromId, User) then
            FromText := User.FirstName + ': ';
        end
        else
        begin
          var Group: TVkGroup;
          if Data.GetGroupById(Item.LastMessage.FromId, Group) then
            FromText := Group.Name + ': ';
        end;
      end;
    end;
      // Текст последнего сообщения
    if not Item.LastMessage.Text.IsEmpty then
      SetMessageText(Item.LastMessage.Text.Replace(#$A, ' ').Replace('  ', ' ', [rfReplaceAll]), False)
      // Вложение
    else if Length(Item.LastMessage.Attachments) > 0 then
    begin
      var AttachText := AttachmentToText(Item.LastMessage.Attachments[0].&Type);
      SetMessageText(AttachText, True);
    end
      // Действие
    else if Assigned(Item.LastMessage.Action) then
    begin
      FromText := '';
      var MemberText := '';

      if PeerIdIsUser(Item.LastMessage.Action.MemberId) then
      begin
        var User: TVkProfile;
        if Data.GetProfileById(Item.LastMessage.Action.MemberId, User) then
          if User.FirstNameAcc.IsEmpty then
            MemberText := User.FullName
          else
            MemberText := User.FullNameAcc;
      end
      else
      begin
        var Group: TVkGroup;
        if Data.GetGroupById(Item.LastMessage.Action.MemberId, Group) then
          MemberText := Group.Name;
      end;

      var ActionFromText := '';
      if PeerIdIsUser(Item.LastMessage.FromId) then
      begin
        var User: TVkProfile;
        if Data.GetProfileById(Item.LastMessage.FromId, User) then
          ActionFromText := User.FullName;
      end
      else
      begin
        var Group: TVkGroup;
        if Data.GetGroupById(Item.LastMessage.FromId, Group) then
          ActionFromText := Group.Name;
      end;

      var ActionText := MessageActionToText(Item.LastMessage.Action, Item.LastMessage.FromId, ActionFromText, MemberText);
      SetMessageText(ActionText, False);
    end
      // Пересланные сообщения
    else if Length(Item.LastMessage.FwdMessages) > 0 then
    begin
      SetMessageText(Length(Item.LastMessage.FwdMessages).ToString + WordOfCount(Length(Item.LastMessage.FwdMessages), [' сообщение', ' сообщения', ' сообщений']), True);
    end;
  end;

  if Assigned(Item.Conversation.PushSettings) then
  begin
    IsMuted := Item.Conversation.PushSettings.NoSound or Item.Conversation.PushSettings.DisabledForever;
  end;

  Unanswered := (Item.Conversation.UnreadCount = 0) and (Item.Conversation.InRead <> Item.Conversation.OutRead);

  if IsSelfChat then
  begin
    Text := 'Избранное';
    Exit;
  end;
  if Item.Conversation.IsChat then
  begin
    if Assigned(Item.Conversation.ChatSettings) then
    begin
      Text := Item.Conversation.ChatSettings.Title;
      if Assigned(Item.Conversation.ChatSettings.Photo) then
        FImageUrl := Item.Conversation.ChatSettings.Photo.Photo50;
    end;
  end
  else if Item.Conversation.IsUser then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.Conversation.Peer.Id, User) then
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
  else if Item.Conversation.IsGroup then
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.Conversation.Peer.Id, Group) then
    begin
      Text := Group.Name;
      FImageUrl := Group.Photo50;
      Verified := Group.Verified;
    end;
  end;
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
  StylesData['info_bottom.Visible'] :=
    StylesData['count_layout.Visible'].AsBoolean or
    StylesData['unread.Visible'].AsBoolean or
    StylesData['mention_layout.Visible'].AsBoolean;
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

procedure TListBoxItemChat.SetIsPinned(const Value: Boolean);
begin
  FIsPinned := Value;
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
  ItemData.Detail := ParseMention(Value);
  if IsAttach then
    StylesData['detail.TextSettings.FontColor'] := $FF71AAEB
  else
    StylesData['detail.TextSettings.FontColor'] := $FF898989;
  StylesData['detail_sel.StartValue'] := StylesData['detail.TextSettings.FontColor'].AsInteger;
end;

procedure TListBoxItemChat.SetText(const Value: string);
begin
  inherited;
end;

procedure TListBoxItemChat.SetUnanswered(const Value: Boolean);
begin
  FUnanswered := Value;
  StylesData['unread.Visible'] := Value;
  StylesData['info_bottom.Visible'] :=
    StylesData['count_layout.Visible'].AsBoolean or
    StylesData['unread.Visible'].AsBoolean or
    StylesData['mention_layout.Visible'].AsBoolean;
end;

procedure TListBoxItemChat.SetUndreadCount(const Value: Integer);
begin
  FUndreadCount := Value;
  StylesData['count_layout.Visible'] := Value > 0;
  StylesData['count'] := Value.ToString;
  StylesData['info_bottom.Visible'] :=
    StylesData['count_layout.Visible'].AsBoolean or
    StylesData['unread.Visible'].AsBoolean or
    StylesData['mention_layout.Visible'].AsBoolean;
end;

procedure TListBoxItemChat.SetVerified(const Value: Boolean);
begin
  FVerified := Value;
  StylesData['verified.Visible'] := FVerified;
end;

end.

