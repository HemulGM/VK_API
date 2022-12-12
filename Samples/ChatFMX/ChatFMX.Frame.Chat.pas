unit ChatFMX.Frame.Chat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Controls.Presentation,
  ChatFMX.DM.Res, FMX.Edit, VK.Types, VK.API, FMX.ImgList, VK.Entity.Message,
  VK.Entity.Conversation, System.Messaging, ChatFMX.Frame.Message,
  HGM.FMX.SmoothScroll, FMX.Ani, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  ChatFMX.Frame.Loading, ChatFMX.Frame.MessageAction, ChatFMX.Frame.MessageDate;

type
  TChatType = (ctChat, ctUser, ctGroup);

  THeadMode = (hmNormal, hmSelection);

  TButton = class(FMX.StdCtrls.TButton)
  protected
    procedure SetText(const Value: string); override;
  end;

  TFrameChat = class(TFrame)
    LayoutClient: TLayout;
    RectangleHead: TRectangle;
    RectangleFooter: TRectangle;
    RectangleChatBG: TRectangle;
    LayoutHead: TLayout;
    LabelTitle: TLabel;
    LabelInfo: TLabel;
    CircleImage: TCircle;
    ButtonActions: TButton;
    ButtonSearch: TButton;
    ButtonCall: TButton;
    ButtonAttchment: TButton;
    LayoutMobileIndicate: TLayout;
    LayoutMuteIndicate: TLayout;
    ImageMobileOnline: TImage;
    Path1: TPath;
    VertScrollBoxMessages: TVertScrollBox;
    LayoutMessageList: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    Layout11: TLayout;
    Layout12: TLayout;
    Circle3: TCircle;
    Layout13: TLayout;
    Text7: TText;
    Layout14: TLayout;
    Text8: TText;
    Text9: TText;
    Layout15: TLayout;
    Layout2: TLayout;
    Label2: TLabel;
    Layout6: TLayout;
    Layout7: TLayout;
    Circle2: TCircle;
    Layout8: TLayout;
    Text4: TText;
    Layout9: TLayout;
    Text5: TText;
    Text6: TText;
    Layout10: TLayout;
    LayoutHeadNormal: TLayout;
    LayoutSelection: TLayout;
    ButtonSelAsSPAM: TButton;
    ButtonSelFavorite: TButton;
    ButtonSelDelete: TButton;
    LabelSelCount: TLabel;
    Layout5: TLayout;
    Path2: TPath;
    LayoutUnsel: TLayout;
    ButtonSelAnswerReply: TButton;
    ButtonSelReply: TButton;
    LayoutSend: TLayout;
    ButtonAudio: TButton;
    ButtonSend: TButton;
    LayoutSendControls: TLayout;
    Layout3: TLayout;
    RectangleMessage: TRectangle;
    Layout4: TLayout;
    MemoText: TMemo;
    LayoutFooterBottom: TLayout;
    LayoutFooterMessage: TLayout;
    LabelTextHint: TLabel;
    LayoutActivity: TLayout;
    FrameLoadingActivity: TFrameLoading;
    LabelActivity: TLabel;
    LayoutActivityContent: TLayout;
    LayoutLoading: TLayout;
    FrameLoadingMesages: TFrameLoading;
    LayoutVerified: TLayout;
    RectangleAddToFriends: TRectangle;
    LayoutAddToFriends: TLayout;
    LabelAddFriendHint: TLabel;
    Button1: TButton;
    Layout16: TLayout;
    PathCloseAddToFriends: TPath;
    RectangleFooterBlock: TRectangle;
    LayoutBlockInfo: TLayout;
    ImageWarning: TImage;
    LabelWarningText: TLabel;
    ButtonBack: TButton;
    procedure VertScrollBoxMessagesResize(Sender: TObject);
    procedure LayoutMessageListResize(Sender: TObject);
    procedure LayoutUnselClick(Sender: TObject);
    procedure MemoTextChangeTracking(Sender: TObject);
    procedure LayoutFooterBottomResize(Sender: TObject);
    procedure VertScrollBoxMessagesViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure ButtonBackClick(Sender: TObject);
  private
    FConversationId: TVkPeerId;
    FVK: TCustomVK;
    FOffset: Int64;
    FOffsetEnd: Boolean;
    FLoading: Boolean;
    FIsMuted: Boolean;
    FTitle: string;
    FImageUrl: string;
    FImageFile: string;
    FChatType: TChatType;
    FMemberCount: Integer;
    FCanCall: Boolean;
    FIsMobile: Boolean;
    FIsOnline: Boolean;
    FLastSeen: TDateTime;
    FIsSelfChat: Boolean;
    FHeadMode: THeadMode;
    FChatScroll: TSmoothScroll;
    FVerified: Boolean;
    FIsNeedAddToFriends: Boolean;
    FUserAccFirstName: string;
    FIsCanWrtie: Boolean;
    FNotAllowedReason: TVkConversationDisableReason;
    FOnBack: TNotifyEvent;
    procedure SetConversationId(const Value: TVkPeerId);
    procedure ReloadAsync;
    procedure SetVK(const Value: TCustomVK);
    procedure EndOfChat;
    procedure LoadConversationAsync;
    procedure LoadConversationInfoAsync;
    procedure CreateMessageItem(const Item: TVkMessage; History: TVkMessageHistory);
    procedure UpdateInfo(Info: TVkConversation; Data: TVkConversations);
    procedure SetIsMuted(const Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetChatType(const Value: TChatType);
    procedure SetMemberCount(const Value: Integer);
    procedure UpdateInfoText;
    procedure SetCanCall(const Value: Boolean);
    procedure SetImageUrl(const Value: string);
    procedure SetIsMobile(const Value: Boolean);
    procedure SetIsOnline(const Value: Boolean);
    procedure SetLastSeen(const Value: TDateTime);
    procedure SetIsSelfChat(const Value: Boolean);
    procedure FOnMessageSelected(Sender: TObject);
    function SelectedCount: Integer;
    procedure UpdateSelection(const Count: Integer);
    procedure SetHeadMode(const Value: THeadMode);
    procedure UpdateFooterSize;
    function GetLastMessage: TFrame;
    function GetMessageDate(Frame: TFrame): TDateTime;
    procedure InsertDateLastMessage(LastDate: TDateTime);
    procedure SetVerified(const Value: Boolean);
    procedure SetIsNeedAddToFriends(const Value: Boolean);
    procedure SetIsCanWrtie(const Value: Boolean);
    procedure SetOnBack(const Value: TNotifyEvent);
    property HeadMode: THeadMode read FHeadMode write SetHeadMode;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    property ConversationId: TVkPeerId read FConversationId write SetConversationId;
    property VK: TCustomVK read FVK write SetVK;
    procedure Load(const PeerId: TVkPeerId);
    property IsMuted: Boolean read FIsMuted write SetIsMuted;
    property Title: string read FTitle write SetTitle;
    property ChatType: TChatType read FChatType write SetChatType;
    property MemberCount: Integer read FMemberCount write SetMemberCount;
    property CanCall: Boolean read FCanCall write SetCanCall;
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property LastSeen: TDateTime read FLastSeen write SetLastSeen;
    property IsOnline: Boolean read FIsOnline write SetIsOnline;
    property IsMobile: Boolean read FIsMobile write SetIsMobile;
    property IsSelfChat: Boolean read FIsSelfChat write SetIsSelfChat;
    procedure UnselectAll;
    property Verified: Boolean read FVerified write SetVerified;
    property IsNeedAddToFriends: Boolean read FIsNeedAddToFriends write SetIsNeedAddToFriends;
    property IsCanWrtie: Boolean read FIsCanWrtie write SetIsCanWrtie;
    property OnBack: TNotifyEvent read FOnBack write SetOnBack;
  end;

implementation

uses
  System.Threading, System.DateUtils, VK.Messages, VK.Entity.Profile,
  VK.Entity.Group, ChatFMX.PreviewManager, System.Math, ChatFMX.Utils,
  HGM.FMX.Image;

{$R *.fmx}

{ TFrameChat }

procedure TFrameChat.ButtonBackClick(Sender: TObject);
begin
  if Assigned(FOnBack) then
    FOnBack(Self);
end;

constructor TFrameChat.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  {$IFDEF ANDROID}
  VertScrollBoxMessages.ShowScrollBars := False;
  RectangleChatBG.Margins.Right := 0;
  RectangleHead.Sides := [];
  RectangleHead.Corners := [];
  RectangleFooterBlock.Sides := [];
  RectangleFooterBlock.Corners := [];
  RectangleFooter.Sides := [];
  RectangleFooter.Corners := [];
  {$ENDIF}
  ButtonBack.Visible := False;
  FChatScroll := TSmoothScroll.CreateFor(VertScrollBoxMessages);
  FChatScroll.ScrollDelta := 2;
  FChatScroll.EnableSmoothScroll := False;
  FLoading := True;
  FVK := AVK;
  Name := '';
  HeadMode := hmNormal;
  MemoText.Text := '';
  MemoText.PrepareForPaint;
  IsNeedAddToFriends := False;
  IsCanWrtie := True;
  UpdateFooterSize;
end;

procedure TFrameChat.EndOfChat;
begin
  if FLoading then
    Exit;
  FLoading := True;
  if not FOffsetEnd then
  begin
    Inc(FOffset, 20);
    TTask.Run(LoadConversationAsync);
  end;
end;

procedure TFrameChat.LayoutFooterBottomResize(Sender: TObject);
begin
  UpdateFooterSize;
end;

procedure TFrameChat.LayoutMessageListResize(Sender: TObject);
begin
  var Sz: Single := 0;
  for var Control in LayoutMessageList.Controls do
    Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  LayoutMessageList.Height := Sz + 10;
  FrameLoadingMesages.Visible := LayoutMessageList.Height + 36 > VertScrollBoxMessages.Height;
end;

procedure TFrameChat.LayoutUnselClick(Sender: TObject);
begin
  UnselectAll;
end;

procedure TFrameChat.Load(const PeerId: TVkPeerId);
begin
  FConversationId := PeerId;
  ReloadAsync;
end;

function TFrameChat.SelectedCount: Integer;
begin
  Result := 0;
  for var Control in LayoutMessageList.Controls do
    if Control is TFrameMessage then
    begin
      if (Control as TFrameMessage).IsSelected then
        Inc(Result);
    end;
end;

procedure TFrameChat.UnselectAll;
begin
  for var Control in LayoutMessageList.Controls do
    if Control is TFrameMessage then
    begin
      (Control as TFrameMessage).BeginUpdate;
      (Control as TFrameMessage).IsSelected := False;
      (Control as TFrameMessage).EndUpdate;
    end;
  UpdateSelection(0);
end;

procedure TFrameChat.UpdateSelection(const Count: Integer);
begin
  if Count <= 0 then
    HeadMode := hmNormal
  else
    HeadMode := hmSelection;
  if HeadMode = hmSelection then
  begin
    LabelSelCount.Text := Count.ToString + ' ' + WordOfCount(Count, ['сообщение', 'сообщения', 'сообщений']);
    if Count > 1 then
      ButtonSelAnswerReply.Text := 'Переслать сюда'
    else
      ButtonSelAnswerReply.Text := 'Ответить';
  end;
end;

procedure TFrameChat.FOnMessageSelected(Sender: TObject);
begin
  UpdateSelection(SelectedCount);
end;

function TFrameChat.GetLastMessage: TFrame;
begin
  for var Control in LayoutMessageList.Controls do
    if (Control is TFrameMessage) or (Control is TFrameMessageAction) then
      Exit(Control as TFrame);
  Result := nil;
end;

function TFrameChat.GetMessageDate(Frame: TFrame): TDateTime;
begin
  Result := 0;
  if not Assigned(Frame) then
    Exit;
  if Frame is TFrameMessage then
    Result := (Frame as TFrameMessage).Date
  else if Frame is TFrameMessageAction then
    Result := (Frame as TFrameMessageAction).Date;
end;

procedure TFrameChat.InsertDateLastMessage(LastDate: TDateTime);
begin
  if LastDate > 0 then
  begin
    var Frame := TFrameMessageDate.Create(LayoutMessageList);
    LayoutMessageList.InsertObject(0, Frame);
    with Frame do
    begin
      Fill(LastDate);
      Align := TAlignLayout.Bottom;
    end;
  end;
end;

procedure TFrameChat.CreateMessageItem(const Item: TVkMessage; History: TVkMessageHistory);
begin
  var LastDate := GetMessageDate(GetLastMessage);
  if LastDate > 0 then
    if not IsSameDay(Item.Date, LastDate) then
      InsertDateLastMessage(LastDate);

  if not Assigned(Item.Action) then
  begin
    var Frame := TFrameMessage.Create(LayoutMessageList, FVK);
    LayoutMessageList.InsertObject(0, Frame);
    with Frame do
    begin
      Fill(Item, History);
      Align := TAlignLayout.Top;
      LayoutMessageList.RecalcSize;
      Align := TAlignLayout.Bottom;
      OnSelectedChanged := FOnMessageSelected;
    end;
  end
  else
  begin
    var Frame := TFrameMessageAction.Create(LayoutMessageList, FVK);
    LayoutMessageList.InsertObject(0, Frame);
    with Frame do
    begin
      Fill(Item, History);
      Align := TAlignLayout.Top;
      LayoutMessageList.RecalcSize;
      Align := TAlignLayout.Bottom;
    end;
  end;
end;

destructor TFrameChat.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameChat.LoadConversationAsync;
var
  Items: TVkMessageHistory;
  Params: TVkParamsMessageHistory;
begin
  Params.Extended;
  Params.Offset(FOffset);
  Params.Count(20);
  Params.Fields([TVkProfileField.Photo50, TVkProfileField.Verified,
    TVkProfileField.FirstNameAcc, TVkProfileField.LastNameAcc], [TVkGroupField.Verified]);
  Params.PeerId(FConversationId);
  if VK.Messages.GetHistory(Items, Params) then
  try
    if Length(Items.Items) < 20 then
      FOffsetEnd := True;
    TThread.Synchronize(nil,
      procedure
      begin
        for var Item in Items.Items do
          CreateMessageItem(Item, Items);
        VertScrollBoxMessagesResize(nil);
        LayoutMessageList.Opacity := 1;
        FLoading := False;
        FrameLoadingMesages.Visible := not FOffsetEnd;
        if FOffsetEnd then
          InsertDateLastMessage(GetMessageDate(GetLastMessage));
      end);
  finally
    Items.Free;
  end;
end;

procedure TFrameChat.UpdateInfo(Info: TVkConversation; Data: TVkConversations);
begin
  ImageUrl := '';
  Title := '';
  IsOnline := False;
  IsMobile := False;
  IsMuted := False;
  CanCall := False;
  IsSelfChat := Info.Peer.Id = FVK.UserId;
  Verified := False;
  LastSeen := 0;
  IsNeedAddToFriends := False;
  if Assigned(Info.CanWrite) then
  begin
    FNotAllowedReason := Info.CanWrite.Reason;
    IsCanWrtie := Info.CanWrite.Allowed
  end
  else
    IsCanWrtie := True;

  if Assigned(Info.PushSettings) then
  begin
    IsMuted := Info.PushSettings.NoSound;
  end;

  if IsSelfChat then
  begin
    try
      var RS: TResourceStream := TResourceStream.Create(HInstance, 'im_favorites_100', RT_RCDATA);
      try
        CircleImage.Fill.Bitmap.Bitmap.LoadFromStream(RS);
      finally
        RS.Free;
      end;
      CircleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      CircleImage.Fill.Kind := TBrushKind.Bitmap;
    except
      CircleImage.Fill.Kind := TBrushKind.Solid;
    end;
    Title := 'Избранное';
    UpdateInfoText;
    Exit;
  end;

  if Info.IsChat then
  begin
    ChatType := ctChat;
    if Assigned(Info.ChatSettings) then
    begin
      Title := Info.ChatSettings.Title;
      if Assigned(Info.ChatSettings.Photo) then
        ImageUrl := Info.ChatSettings.Photo.Photo50;
      MemberCount := Info.ChatSettings.MembersCount;
      if Assigned(Info.ChatSettings.ACL) then
        CanCall := Info.ChatSettings.ACL.CanCall;
    end;
  end
  else if Info.IsUser then
  begin
    ChatType := ctUser;
    var User: TVkProfile;
    if Data.GetProfileById(Info.Peer.Id, User) then
    begin
      Title := User.FullName;
      ImageUrl := User.Photo50;
      Verified := User.Verified;
      FUserAccFirstName := User.FirstNameAcc;
      IsNeedAddToFriends := (not User.IsFriend) and (User.CanSendFriendRequest);
      if Assigned(User.OnlineInfo) then
      begin
        LastSeen := User.OnlineInfo.LastSeen;
        IsOnline := User.OnlineInfo.IsOnline;
        IsMobile := User.OnlineInfo.IsMobile;
      end;
    end;
  end
  else if Info.IsGroup then
  begin
    ChatType := ctGroup;
    var Group: TVkGroup;
    if Data.GetGroupById(Info.Peer.Id, Group) then
    begin
      Title := Group.Name;
      ImageUrl := Group.Photo50;
      Verified := Group.Verified;
    end;
  end;
end;

procedure TFrameChat.UpdateInfoText;
begin
  if IsSelfChat then
  begin
    LabelInfo.Text := '';
    Exit;
  end;
  case FChatType of
    ctChat:
      begin
        if FMemberCount = 0 then
          LabelInfo.Text := ''
        else
          LabelInfo.Text := FMemberCount.ToString + ' ' + WordOfCount(FMemberCount, ['участник', 'участника', 'участников']);
      end;
    ctUser:
      begin
        if IsOnline then
          LabelInfo.Text := 'online'
        else if LastSeen <> 0 then
          LabelInfo.Text := 'был(а) в сети ' + HumanDateTime(LastSeen, True)
        else
          LabelInfo.Text := 'был(а) в сети недавно';
      end;
    ctGroup:
      begin
        LabelInfo.Text := '';
      end;
  end;
  LayoutMobileIndicate.Visible := FIsMobile;
end;

procedure TFrameChat.VertScrollBoxMessagesResize(Sender: TObject);
begin
  if LayoutMessageList.Width <> VertScrollBoxMessages.Width then
    LayoutMessageList.Width := VertScrollBoxMessages.ClientWidth;
  LayoutMessageList.Position.Y := VertScrollBoxMessages.Height - LayoutMessageList.Height;
end;

procedure TFrameChat.VertScrollBoxMessagesViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  var Content := VertScrollBoxMessages.Content.ScrollBox.ContentBounds;
  if Abs(NewViewportPosition.Y - Content.Top) < 500 then
    EndOfChat;
end;

procedure TFrameChat.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if not FImageFile.IsEmpty then
  try
    CircleImage.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    CircleImage.Fill.Kind := TBrushKind.Bitmap;
    CircleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleImage.Fill.Kind := TBrushKind.Solid;
  end
  else
    CircleImage.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameChat.LoadConversationInfoAsync;
var
  Params: TVkParamsConversationsGetById;
  Items: TVkConversations;
begin
  Params.PeerId(FConversationId);
  Params.Extended;
  Params.Fields([TVkGroupField.Verified, TVkGroupField.Photo50],
    [TVkProfileField.OnlineInfo, TVkProfileField.FirstNameAcc,
    TVkProfileField.IsFriend, TVkProfileField.CanSendFriendRequest,
    TVkProfileField.CanWritePrivateMessage]);
  if VK.Messages.GetConversationsById(Items, Params) then
  try
    if Length(Items.Items) > 0 then
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          UpdateInfo(Items.Items[0], Items);
          LayoutHead.Opacity := 1;
        end);
    end;
  finally
    Items.Free;
  end;
end;

procedure TFrameChat.MemoTextChangeTracking(Sender: TObject);
begin
  UpdateFooterSize;
  LabelTextHint.Visible := MemoText.Text.IsEmpty;
  ButtonAudio.Visible := MemoText.Text.IsEmpty;
  ButtonSend.Visible := not MemoText.Text.IsEmpty;
end;

procedure TFrameChat.UpdateFooterSize;
var
  H: Single;
begin
  H :=
    Max(36, Min(MemoText.ContentSize.Height + 20, 220)) +
    RectangleMessage.Margins.Top +
    RectangleMessage.Margins.Bottom;
  if LayoutFooterBottom.Visible then
    H := H + LayoutFooterBottom.Height;
  RectangleFooter.Height := H;
end;

procedure TFrameChat.ReloadAsync;
begin
  LayoutMessageList.BeginUpdate;
  try
    LayoutActivity.Parent := nil;
    LayoutLoading.Parent := nil;
    while LayoutMessageList.ControlsCount > 0 do
      LayoutMessageList.Controls[0].Free;
    LayoutActivity.Parent := LayoutMessageList;
    LayoutLoading.Parent := LayoutMessageList;
  finally
    LayoutMessageList.EndUpdate;
  end;
  LayoutMessageList.Position.Y := 0;
  LayoutMessageList.Height := VertScrollBoxMessages.Height;
  LayoutMessageList.Opacity := 0;

  LabelTitle.Text := 'Загрузка...';
  LabelInfo.Text := '';
  LayoutHead.Opacity := 0;
  LayoutMobileIndicate.Visible := False;
  LayoutMuteIndicate.Visible := False;

  FOffset := 0;
  FOffsetEnd := False;
  //
  FLoading := True;
  TTask.Run(
    procedure
    begin
      LoadConversationInfoAsync;
      LoadConversationAsync;
      FLoading := False;
    end);
end;

procedure TFrameChat.SetCanCall(const Value: Boolean);
begin
  FCanCall := Value;
  ButtonCall.Visible := FCanCall;
end;

procedure TFrameChat.SetChatType(const Value: TChatType);
begin
  FChatType := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetConversationId(const Value: TVkPeerId);
begin
  FConversationId := Value;
end;

procedure TFrameChat.SetHeadMode(const Value: THeadMode);
begin
  FHeadMode := Value;
  LayoutSelection.Visible := FHeadMode = hmSelection;
  LayoutHeadNormal.Visible := FHeadMode = hmNormal;
end;

procedure TFrameChat.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if IsSelfChat then
    Exit;
  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    CircleImage.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameChat.SetIsCanWrtie(const Value: Boolean);
begin
  FIsCanWrtie := Value;
  RectangleFooter.Visible := FIsCanWrtie;
  RectangleFooterBlock.Visible := not FIsCanWrtie;
  if RectangleFooterBlock.Visible then
  begin
    ImageWarning.Bitmap.LoadFromResource('msg_warning');
  end;
  var Reason: string := '';
  case FNotAllowedReason of
    TVkConversationDisableReason.UserBannedOrDeleted:
      Reason := 'Пользователь удалён.';
    TVkConversationDisableReason.UserBlacklisted:
      Reason := 'Нельзя отправить сообщение пользователю, который в чёрном списке';
    TVkConversationDisableReason.UserDisableGroupsMessages:
      Reason := 'Пользователь запретил сообщения от сообщества';
    TVkConversationDisableReason.UserPrivacy:
      Reason := 'Вы не можете отправить сообщение этому пользователю, поскольку он ограничил круг лиц, которые могут присылать ему сообщения.';
    TVkConversationDisableReason.GroupDisableMessages:
      Reason := 'Сообщество отключило сообщения.';
    TVkConversationDisableReason.GroupBannedMessages:
      Reason := 'В сообществе заблокированы сообщения';
    TVkConversationDisableReason.NoAccessChat:
      Reason := 'Вы были исключены из этого чата.';
    TVkConversationDisableReason.NoAccessEMail:
      Reason := 'Нет доступа к e-mail';
    TVkConversationDisableReason.NoAccessGroup:
      Reason := 'Вы не можете отправить сообщение этому сообществу, поскольку оно ограничило круг лиц, которые могут присылать ему сообщения.';
    TVkConversationDisableReason.Forbidden:
      Reason := 'Отправка сообщений ограничена';
  else
    Reason := 'Отправка сообщений ограничена';
  end;
  LabelWarningText.Text := Reason;
end;

procedure TFrameChat.SetIsMobile(const Value: Boolean);
begin
  FIsMobile := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetIsMuted(const Value: Boolean);
begin
  FIsMuted := Value;
  LayoutMuteIndicate.Visible := FIsMuted;
end;

procedure TFrameChat.SetIsNeedAddToFriends(const Value: Boolean);
begin
  FIsNeedAddToFriends := Value;
  RectangleAddToFriends.Visible := FIsNeedAddToFriends;
  if FIsNeedAddToFriends then
  begin
    LabelAddFriendHint.Text := 'Добавьте ' + FUserAccFirstName + ' в друзья и общайтесь чаще';
  end;
end;

procedure TFrameChat.SetIsOnline(const Value: Boolean);
begin
  FIsOnline := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetIsSelfChat(const Value: Boolean);
begin
  FIsSelfChat := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetLastSeen(const Value: TDateTime);
begin
  FLastSeen := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetMemberCount(const Value: Integer);
begin
  FMemberCount := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetOnBack(const Value: TNotifyEvent);
begin
  FOnBack := Value;
  ButtonBack.Visible := Assigned(FOnBack);
end;

procedure TFrameChat.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

procedure TFrameChat.SetVerified(const Value: Boolean);
begin
  FVerified := Value;
  LayoutVerified.Visible := FVerified;
end;

procedure TFrameChat.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

{ TButton }

procedure TButton.SetText(const Value: string);
begin
  inherited;
  if Tag = 15 then
    if Assigned(Canvas) then
      Width := Canvas.TextWidth(Value) + 35;
end;

end.

