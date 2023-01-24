unit ChatFMX.Frame.Chat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Controls.Presentation,
  ChatFMX.DM.Res, FMX.Edit, VK.Types, VK.API, FMX.ImgList, VK.Entity.Message,
  VK.Entity.Conversation, System.Messaging, HGM.FMX.SmoothScroll, FMX.Ani,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, ChatFMX.Frame.Loading,
  VK.Entity.Common.ExtendedList, ChatFMX.Frame.Message, ChatFMX.Classes,
  ChatFMX.Frame.Attachment.PinnedMessage;

type
  TFrameChat = class;

  TLayout = class(FMX.Layouts.TLayout)
  end;

  THeadMode = (hmNormal, hmSelection);

  TButton = class(FMX.StdCtrls.TButton)
  protected
    procedure SetText(const Value: string); override;
  end;

  TForEachMessage = reference to procedure(Item: TFrameMessage; var Break: Boolean);

  TOnPreviewChatChanged = procedure(Chat: TFrameChat) of object;

  TFrameMessages = TArray<TFrameMessage>;

  TFrameMessagesHelper = record helper for TFrameMessages
    function ToIds: TArrayOfInteger;
  end;

{$IFDEF DEBUG_ADAPTIVE}
{$DEFINE ANDROID}
{$ENDIF}

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
    LayoutBottomHints: TLayout;
    CircleToDown: TCircle;
    Path3: TPath;
    ColorAnimationToDown: TColorAnimation;
    RectanglePinnedMessage: TRectangle;
    LayoutPinnedMessageClient: TLayout;
    Layout2: TLayout;
    PathUnpinMessage: TPath;
    Layout6: TLayout;
    Path5: TPath;
    ButtonSelPin: TButton;
    LayoutFooterTop: TLayout;
    procedure VertScrollBoxMessagesResize(Sender: TObject);
    procedure LayoutMessageListResize(Sender: TObject);
    procedure LayoutUnselClick(Sender: TObject);
    procedure MemoTextChangeTracking(Sender: TObject);
    procedure LayoutFooterBottomResize(Sender: TObject);
    procedure VertScrollBoxMessagesViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure ButtonBackClick(Sender: TObject);
    procedure CircleToDownClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure MemoTextKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure PathCloseAddToFriendsClick(Sender: TObject);
    procedure PathUnpinMessageClick(Sender: TObject);
    procedure ButtonSelPinClick(Sender: TObject);
    procedure ButtonSelFavoriteClick(Sender: TObject);
    procedure ButtonSelDeleteClick(Sender: TObject);
    procedure ButtonSelAsSPAMClick(Sender: TObject);
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
    FIsUser: Boolean;
    FOutRead: Int64;
    FHavePinned: Boolean;
    FPinnedMessage: TFrameAttachmentPinnedMessage;
    FPinnedMessageId: Int64;
    FIsCanPinMessage: Boolean;
    FUserSex: TVkSex;
    procedure SetConversationId(const Value: TVkPeerId);
    procedure ReloadAsync;
    procedure SetVK(const Value: TCustomVK);
    procedure EndOfChat;
    procedure LoadConversationAsync;
    procedure LoadConversationInfoAsync;
    procedure CreateMessageItem(const Item: TVkMessage; AData: TVkEntityExtendedList<TVkMessage>; IsNew: Boolean);
    procedure UpdateInfo(Info: TVkConversation; Data: IExtended);
    procedure SetIsMuted(const Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnNewMessage(const Sender: TObject; const M: TMessage);
    procedure FOnEditMessage(const Sender: TObject; const M: TMessage);
    procedure FOnDeleteMessage(const Sender: TObject; const M: TMessage);
    procedure FOnChangeMessage(const Sender: TObject; const M: TMessage);
    procedure FOnReadMessage(const Sender: TObject; const M: TMessage);
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
    procedure AppendHistory(Items: TVkMessageHistory);
    procedure CalcDisappear;
    procedure ShowHints;
    procedure HideHints;
    procedure ErrorLoading;
    procedure AppendMessage(Items: TVkMessages);
    function GetChatInfo: TChatInfo;
    procedure SetIsUser(const Value: Boolean);
    procedure SetOutRead(const Value: Int64);
    procedure SendMessage;
    procedure SetHavePinned(const Value: Boolean);
    procedure CreatePinnedMessage(Item: TVkMessage; Data: IExtended);
    function GetSelected: TArray<TFrameMessage>;
    procedure UpdatePinMessage(const MessageId: Int64; IsLocalId: Boolean);
    procedure DoAction(Action: TVkMessageAction);
    procedure SetPinnedMessageId(const Value: Int64);
    procedure SetIsCanPinMessage(const Value: Boolean);
    procedure UpdatePinButton(const Count: Integer; const Selected: TArray<TFrameMessage>);
    procedure UpdateFavoriteButton(const Count: Integer; const Selected: TArray<TFrameMessage>);
    procedure UpdateReplyAnswerButton(const Count: Integer);
    procedure ForEach(Proc: TForEachMessage);
    procedure SetUserSex(const Value: TVkSex);
    property HeadMode: THeadMode read FHeadMode write SetHeadMode;
  protected
    procedure SetVisible(const Value: Boolean); override;
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
    property IsUser: Boolean read FIsUser write SetIsUser;
    property OnBack: TNotifyEvent read FOnBack write SetOnBack;
    property ChatInfo: TChatInfo read GetChatInfo;
    property OutRead: Int64 read FOutRead write SetOutRead;
    property HavePinned: Boolean read FHavePinned write SetHavePinned;
    property PinnedMessageId: Int64 read FPinnedMessageId write SetPinnedMessageId;
    property IsCanPinMessage: Boolean read FIsCanPinMessage write SetIsCanPinMessage;
    property UserSex: TVkSex read FUserSex write SetUserSex;
  end;

const
  ChatCountQuery = 40;
  ImageIndexFavoriteOn = 22;
  ImageIndexFavoriteOff = 9;
  ImageIndexPinOn = 21;
  ImageIndexPinOff = 20;

implementation

uses
  System.Threading, System.DateUtils, VK.Messages, VK.Entity.Profile,
  VK.Entity.Group, ChatFMX.PreviewManager, System.Math, ChatFMX.Utils,
  HGM.FMX.Image, ChatFMX.Frame.MessageAction, ChatFMX.Frame.MessageDate,
  ChatFMX.Events;

{$R *.fmx}
{ TFrameChat }

function TFrameChat.GetSelected: TArray<TFrameMessage>;
begin
  var
    Cnt := 0;
  SetLength(Result, LayoutMessageList.ControlsCount);
  for var Control in LayoutMessageList.Controls do
    if (Control is TFrameMessage) and (Control as TFrameMessage).IsSelected then
    begin
      Result[Cnt] := Control as TFrameMessage;
      Inc(Cnt);
    end;
  SetLength(Result, Cnt);
end;

procedure TFrameChat.ButtonBackClick(Sender: TObject);
begin
  if Assigned(FOnBack) then
    FOnBack(Self);
end;

procedure TFrameChat.UpdatePinMessage(const MessageId: Int64; IsLocalId: Boolean);
begin
  TTask.Run(
    procedure
    var
      Items: TVkMessages;
      Params: TVkParamsMessageGet;
      ParamsLocal: TVkParamsMessageGetByConvMesId;
    begin
      Items := nil;
      if IsLocalId then
      begin
        ParamsLocal.PeerId(ConversationId);
        ParamsLocal.ConversationMessageId(MessageId);
        ParamsLocal.Extended;
        VK.Messages.GetByConversationMessageId(Items, ParamsLocal);
      end
      else
      begin
        Params.MessageId(MessageId);
        Params.Extended;
        VK.Messages.GetById(Items, Params);
      end;
      if Assigned(Items) then
      begin
        var
          Extended: IExtended := Items;
        if Length(Items.Items) > 0 then
          TThread.Synchronize(nil,
            procedure
            begin
              CreatePinnedMessage(Items.Items[0], Extended);
            end);
      end;
      Items := nil;
    end);
end;

procedure TFrameChat.ButtonSelAsSPAMClick(Sender: TObject);
begin
  if SelectedCount <= 0 then
    Exit;
  var
    Ids := GetSelected.ToIds;
  TTask.Run(
    procedure
    begin
      var
        MessageIds: TVkMessageDelete;
      if VK.Messages.Delete(MessageIds, Ids, 0, False, True) then
        MessageIds.Free;
    end);
  UnselectAll;
end;

procedure TFrameChat.ButtonSelDeleteClick(Sender: TObject);
begin
  if SelectedCount <= 0 then
    Exit;
  var
    Ids := GetSelected.ToIds;
  TTask.Run(
    procedure
    begin
      var
        MessageIds: TVkMessageDelete;
      if VK.Messages.Delete(MessageIds, Ids, 0, False, False) then
        MessageIds.Free;
    end);
  UnselectAll;
end;

procedure TFrameChat.ButtonSelFavoriteClick(Sender: TObject);
begin
  if SelectedCount <= 0 then
    Exit;
  var
    Ids := GetSelected.ToIds;
  case ButtonSelFavorite.ImageIndex of
    ImageIndexFavoriteOff:
      TTask.Run(
        procedure
        begin
          var
            MessageIds: TIdList;
          VK.Messages.MarkAsImportant(MessageIds, Ids, True);
        end);
    ImageIndexFavoriteOn:
      TTask.Run(
        procedure
        begin
          var
            MessageIds: TIdList;
          VK.Messages.MarkAsImportant(MessageIds, Ids, False);
        end);
  end;
  UnselectAll;
end;

procedure TFrameChat.ButtonSelPinClick(Sender: TObject);
begin
  if SelectedCount <> 1 then
    Exit;
  var
    Frame := GetSelected[0];
  case ButtonSelPin.ImageIndex of
    ImageIndexPinOff:
      TTask.Run(
        procedure
        begin
          VK.Messages.Pin(ConversationId, Frame.MessageId);
        end);
    ImageIndexPinOn:
      TTask.Run(
        procedure
        begin
          VK.Messages.Unpin(ConversationId);
        end);
  end;
  UnselectAll;
end;

procedure TFrameChat.ButtonSendClick(Sender: TObject);
begin
  SendMessage;
end;

procedure TFrameChat.SendMessage;
begin
  var
    NewMessage := VK.Messages.New;
  NewMessage.PeerId(ConversationId);
  if not MemoText.Text.IsEmpty then
  begin
    NewMessage.Message(MemoText.Text);
    MemoText.Text := '';
    MemoText.SetFocus;
  end;
  RectangleFooter.Enabled := False;
  TTask.Run(
    procedure
    begin
      NewMessage.Send;
      TThread.Queue(nil,
        procedure
        begin
          RectangleFooter.Enabled := True;
        end);
    end);
end;

constructor TFrameChat.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FPinnedMessage := nil;
  FPinnedMessageId := -1;
  FChatScroll := TSmoothScroll.CreateFor(VertScrollBoxMessages);
{$IFDEF ANDROID}
  VertScrollBoxMessages.ShowScrollBars := False;
  RectangleChatBG.Margins.Right := 0;
  RectangleHead.Sides := [];
  RectangleHead.Corners := [];
  RectangleFooterBlock.Sides := [];
  RectangleFooterBlock.Corners := [];
  RectangleFooter.Sides := [];
  RectangleFooter.Corners := [];
{$ELSE}
  FChatScroll.ScrollDelta := 2;
  FChatScroll.EnableSmoothScroll := False;
{$ENDIF}
  ButtonBack.Visible := False;
  FLoading := True;
  FVK := AVK;
  Name := '';
  HeadMode := hmNormal;
  MemoText.Text := '';
  MemoText.PrepareForPaint;
  IsNeedAddToFriends := False;
  IsCanWrtie := True;
  HavePinned := False;
  UpdateFooterSize;
end;

procedure TFrameChat.EndOfChat;
begin
  if FLoading then
    Exit;
  if not FOffsetEnd then
  begin
    FLoading := True;
    Inc(FOffset, ChatCountQuery);
    TTask.Run(LoadConversationAsync);
  end;
  FrameLoadingMesages.Visible := not FOffsetEnd;
end;

procedure TFrameChat.LayoutFooterBottomResize(Sender: TObject);
begin
  UpdateFooterSize;
end;

procedure TFrameChat.LayoutMessageListResize(Sender: TObject);
begin
  LayoutMessageList.Realign;
  var Sz: Single := 0;
  for var Control in LayoutMessageList.Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  LayoutMessageList.Height := Sz + 10;
  FrameLoadingMesages.Visible :=
    (LayoutMessageList.Height + 36 > VertScrollBoxMessages.Height) and
    (not FOffsetEnd);
  {
    LayoutMessageList.Sort(
    function(Left, Right: TFmxObject): Integer
    begin
    var DL: TDateTime := 0;
    if Left is TFrameMessageAction then
    DL := (Left as TFrameMessageAction).Date
    else if Left is TFrameMessage then
    DL := (Left as TFrameMessage).Date
    else if Left is TFrameMessageDate then
    DL := (Left as TFrameMessageDate).Date;

    var DR: TDateTime := 0;
    if Right is TFrameMessageAction then
    DR := (Right as TFrameMessageAction).Date
    else if Right is TFrameMessage then
    DR := (Right as TFrameMessage).Date
    else if Right is TFrameMessageDate then
    DR := (Right as TFrameMessageDate).Date;

    Result := CompareDate(DL, DR);
    end);
    LayoutMessageList.FNeedAlign := True;
    LayoutMessageList.Realign; }
         {
  LayoutMessageList.Sort(
    function(Left, Right: TFMXObject): integer
    begin
      var DL: TDateTime := 0;
      if Left is TFrameMessageAction then
        DL := (Left as TFrameMessageAction).Date
      else if Left is TFrameMessage then
        DL := (Left as TFrameMessage).Date
      else if Left is TFrameMessageDate then
        DL := (Left as TFrameMessageDate).Date;

      var DR: TDateTime := 0;
      if Right is TFrameMessageAction then
        DR := (Right as TFrameMessageAction).Date
      else if Right is TFrameMessage then
        DR := (Right as TFrameMessage).Date
      else if Right is TFrameMessageDate then
        DR := (Right as TFrameMessageDate).Date;

      if DL < DR then
      begin
        (Left as TControl).Position.Y := (Right as TControl).Position.Y - (Left as TControl).Height - 1;
        Result := -1
      end
      else if DL > DR then
      begin
        (Left as TControl).Position.Y := (Right as TControl).Position.Y + (Right as TControl).Height + 1;
        Result := 1;
      end
      else
        Result := 0;
    end);  }
end;

procedure TFrameChat.LayoutUnselClick(Sender: TObject);
begin
  UnselectAll;
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

procedure TFrameChat.UpdatePinButton(const Count: Integer; const Selected: TArray<TFrameMessage>);
begin
  ButtonSelPin.Visible := FIsCanPinMessage and (Count = 1);
  if ButtonSelPin.Visible then
  begin
    if Selected[0].MessageId = PinnedMessageId then
      ButtonSelPin.ImageIndex := ImageIndexPinOn
    else
      ButtonSelPin.ImageIndex := ImageIndexPinOff;
    ButtonSelPin.Align := TAlignLayout.Left;
    ButtonSelPin.Align := TAlignLayout.Right;
  end;
end;

procedure TFrameChat.UpdateFavoriteButton(const Count: Integer; const Selected: TArray<TFrameMessage>);
begin
  var
    AllFavorite := True;
  for var Item in Selected do
    if not Item.IsImportant then
    begin
      AllFavorite := False;
      Break;
    end;
  if AllFavorite then
    ButtonSelFavorite.ImageIndex := ImageIndexFavoriteOn
  else
    ButtonSelFavorite.ImageIndex := ImageIndexFavoriteOff;
end;

procedure TFrameChat.UpdateReplyAnswerButton(const Count: Integer);
begin
  if Count > 1 then
    ButtonSelAnswerReply.Text := 'Переслать сюда'
  else
    ButtonSelAnswerReply.Text := 'Ответить';
end;

procedure TFrameChat.UpdateSelection(const Count: Integer);
begin
  if Count <= 0 then
    HeadMode := hmNormal
  else
  begin
    HeadMode := hmSelection;
    var
      Selected := GetSelected;
    UpdatePinButton(Count, Selected);
    UpdateFavoriteButton(Count, Selected);
    UpdateReplyAnswerButton(Count);
    LabelSelCount.Text := Count.ToString + ' ' +
      WordOfCount(Count, ['сообщение', 'сообщения', 'сообщений']);
  end;
end;

procedure TFrameChat.ForEach(Proc: TForEachMessage);
begin
  var
    DoBreak := False;
  for var Control in LayoutMessageList.Controls do
    if Control is TFrameMessage then
    begin
      Proc(Control as TFrameMessage, DoBreak);
      if DoBreak then
        Exit;
    end;
end;

procedure TFrameChat.FOnChangeMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventMessageChange absolute M;
begin
  if Event.Data.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  var
    MessageId := Event.Data.MessageId;
  ForEach(
    procedure(Item: TFrameMessage; var Break: Boolean)
    begin
      if Item.MessageId = MessageId then
      begin
        Item.UpdateFlags(Event.Data.ChangeType, Event.Data.Flags);
        Break := True;
      end;
    end);
end;

procedure TFrameChat.FOnDeleteMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventDeleteMessage absolute M;
  MessageId: Int64;
begin
  if Event.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  MessageId := Event.LocalId;
  ForEach(
    procedure(Item: TFrameMessage; var Break: Boolean)
    begin
      if Item.ConversationMessageId = MessageId then
      begin
        Item.IsDeleted := True;
        Break := True;
      end;
    end);
end;

procedure TFrameChat.FOnReadMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventReadMessages absolute M;
  MessageId: Int64;
begin
  if Event.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  MessageId := Event.LocalId;
  ForEach(
    procedure(Item: TFrameMessage; var Break: Boolean)
    begin
      if Item.MessageId >= MessageId then
        Item.IsUnread := False;
    end);
end;

procedure TFrameChat.FOnEditMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventEditMessage absolute M;
begin
  if Event.Data.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
end;

procedure TFrameChat.FOnNewMessage(const Sender: TObject; const M: TMessage);
var
  Event: TEventNewMessage absolute M;
  MessageId: Int64;
begin
  if Event.Data.PeerId <> NormalizePeerId(FConversationId) then
    Exit;
  MessageId := Event.Data.MessageId;
  TTask.Run(
    procedure
    var
      Items: TVkMessages;
      Params: TVkParamsMessageGet;
    begin
      Params.MessageId(MessageId);
      Params.Extended;
      Params.Fields([TVkExtendedField.Photo50, TVkExtendedField.Verified,
        TVkExtendedField.Sex, TVkExtendedField.FirstNameAcc,
        TVkExtendedField.LastNameAcc]);
      if VK.Messages.GetById(Items, Params) then
      try
        TThread.Synchronize(nil,
          procedure
          begin
            AppendMessage(Items);
          end);
      finally
        Items.Free;
      end;
    end);
end;

procedure TFrameChat.AppendMessage(Items: TVkMessages);
begin
  for var Item in Items.Items do
    CreateMessageItem(Item, Items, True);

  VertScrollBoxMessagesResize(nil);
  LayoutMessageListResize(nil);
  VertScrollBoxMessagesResize(nil);
end;

procedure TFrameChat.FOnMessageSelected(Sender: TObject);
begin
  UpdateSelection(SelectedCount);
end;

function TFrameChat.GetChatInfo: TChatInfo;
begin
  Result.IsCanWrite := IsCanWrtie;
  Result.IsP2P := IsUser;
  Result.OutRead := OutRead;
  Result.PeerId := ConversationId;
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
    var
      Frame := TFrameMessageDate.Create(LayoutMessageList);
    LayoutMessageList.InsertObject(0, Frame);
    with Frame do
    begin
      Fill(LastDate);
      Align := TAlignLayout.Bottom;
    end;
  end;
end;

procedure TFrameChat.CreateMessageItem(const Item: TVkMessage; AData: TVkEntityExtendedList<TVkMessage>; IsNew: Boolean);
begin
  if not IsNew then
  begin
    var
      LastDate := GetMessageDate(GetLastMessage);
    if LastDate > 0 then
      if not IsSameDay(Item.Date, LastDate) then
        InsertDateLastMessage(LastDate);
  end;

  if not Assigned(Item.Action) then
  begin
    var
      Frame := TFrameMessage.Create(LayoutMessageList, FVK);
    if not IsNew then
    begin
      Frame.Align := TAlignLayout.MostTop;
      LayoutMessageList.InsertObject(0, Frame);
    end
    else
    begin
      Frame.Align := TAlignLayout.MostBottom;
      LayoutMessageList.AddObject(Frame);
    end;
    Frame.Fill(Item, AData, ChatInfo);
    LayoutMessageList.RecalcSize;
    Frame.Align := TAlignLayout.Bottom;
    Frame.OnSelectedChanged := FOnMessageSelected;
  end
  else
  begin
    if IsNew then
      DoAction(Item.Action);

    var
      Frame := TFrameMessageAction.Create(LayoutMessageList, FVK);
    if not IsNew then
    begin
      Frame.Align := TAlignLayout.MostTop;
      LayoutMessageList.InsertObject(0, Frame);
    end
    else
    begin
      Frame.Align := TAlignLayout.MostBottom;
      LayoutMessageList.AddObject(Frame);
    end;
    Frame.Fill(Item, AData, ChatInfo);
    LayoutMessageList.RecalcSize;
    Frame.Align := TAlignLayout.Bottom;
  end;
end;

procedure TFrameChat.DoAction(Action: TVkMessageAction);
begin
  case Action.&Type of
    TVkMessageActionType.ChatPhotoUpdate:
      TTask.Run(LoadConversationInfoAsync);
    TVkMessageActionType.ChatPhotoRemove:
      TTask.Run(LoadConversationInfoAsync);
    TVkMessageActionType.ChatTitleUpdate:
      Title := Action.Text;
    TVkMessageActionType.ChatPinMessage:
      UpdatePinMessage(Action.ConversationMessageId, True);
    TVkMessageActionType.ChatUnpinMessage:
      HavePinned := False;
    TVkMessageActionType.ChatInviteUser:
      MemberCount := MemberCount + 1;
    TVkMessageActionType.ChatKickUser:
      MemberCount := MemberCount - 1;
    TVkMessageActionType.ChatInviteUserByLink:
      MemberCount := MemberCount + 1;
    TVkMessageActionType.ConversationStyleUpdate:
      TTask.Run(LoadConversationInfoAsync);
  end;
end;

procedure TFrameChat.Load(const PeerId: TVkPeerId);
begin
  FConversationId := PeerId;
  Event.Subscribe(TEventNewMessage, FOnNewMessage);
  Event.Subscribe(TEventEditMessage, FOnEditMessage);
  Event.Subscribe(TEventDeleteMessage, FOnDeleteMessage);
  Event.Subscribe(TEventMessageChange, FOnChangeMessage);
  Event.Subscribe(TEventReadMessages, FOnReadMessage);
  ReloadAsync;
end;

destructor TFrameChat.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  TPreview.Instance.Unsubscribe(FOnNewMessage);
  TPreview.Instance.Unsubscribe(FOnEditMessage);
  TPreview.Instance.Unsubscribe(FOnDeleteMessage);
  TPreview.Instance.Unsubscribe(FOnChangeMessage);
  TPreview.Instance.Unsubscribe(FOnReadMessage);
  inherited;
end;

procedure TFrameChat.AppendHistory(Items: TVkMessageHistory);
begin
  for var Item in Items.Items do
    CreateMessageItem(Item, Items, False);
  if FOffsetEnd then
    InsertDateLastMessage(GetMessageDate(GetLastMessage));
  LayoutMessageList.Visible := True;
  VertScrollBoxMessagesResize(nil);
  LayoutMessageListResize(nil);
  VertScrollBoxMessagesResize(nil);
end;

procedure TFrameChat.LoadConversationAsync;
var
  Items: TVkMessageHistory;
  Params: TVkParamsMessageHistory;
begin
  Params.Extended;
  Params.Offset(FOffset);
  Params.Count(ChatCountQuery);
  Params.Fields([TVkExtendedField.Photo50, TVkExtendedField.Verified,
    TVkExtendedField.Sex, TVkExtendedField.FirstNameAcc, TVkExtendedField.FirstNameGen,
    TVkExtendedField.LastNameGen,
    TVkExtendedField.LastNameAcc]);
  Params.PeerId(FConversationId);
  try
    if VK.Messages.GetHistory(Items, Params) then
    try
      if Length(Items.Items) < ChatCountQuery then
        FOffsetEnd := True;
      TThread.Synchronize(nil,
        procedure
        begin
          AppendHistory(Items);
          FLoading := False;
        end);
    finally
      Items.Free;
    end;
  except
    TThread.Synchronize(nil,
      procedure
      begin
        FLoading := False;
        Dec(FOffset, ChatCountQuery);
        ErrorLoading;
      end);
  end;
end;

procedure TFrameChat.ErrorLoading;
begin

end;

procedure TFrameChat.CreatePinnedMessage(Item: TVkMessage; Data: IExtended);
begin
  if Assigned(FPinnedMessage) then
    FPinnedMessage.Free;
  FPinnedMessageId := Item.Id;
  FPinnedMessage := TFrameAttachmentPinnedMessage.Create
    (LayoutPinnedMessageClient, FVK);
  // FPinnedMessage.OnClick := FOnPinnedClick;
  FPinnedMessage.Parent := LayoutPinnedMessageClient;
  FPinnedMessage.Align := TAlignLayout.Client;
  FPinnedMessage.Fill(Item, Data);
  HavePinned := True;
end;

procedure TFrameChat.UpdateInfo(Info: TVkConversation; Data: IExtended);
begin
  ImageUrl := '';
  Title := '';
  IsOnline := False;
  IsMobile := False;
  IsMuted := False;
  CanCall := False;
  IsSelfChat := Info.Peer.Id = FVK.UserId;
  Verified := False;
  HavePinned := False;
  FIsCanPinMessage := False;
  LastSeen := 0;
  IsNeedAddToFriends := False;
  if Assigned(Info.CanWrite) then
  begin
    FNotAllowedReason := Info.CanWrite.Reason;
    IsCanWrtie := Info.CanWrite.Allowed
  end
  else
    IsCanWrtie := True;

  IsUser := Info.IsUser;
  OutRead := Info.OutRead;

  if Assigned(Info.PushSettings) then
  begin
    IsMuted := Info.PushSettings.NoSound;
  end;

  if IsSelfChat then
  begin
    try
      var
        RS: TResourceStream := TResourceStream.Create(HInstance,
        'im_favorites_100', RT_RCDATA);
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
      if Assigned(Info.ChatSettings.PinnedMessage) then
        CreatePinnedMessage(Info.ChatSettings.PinnedMessage, Data);
      FIsCanPinMessage := True;
    end;
  end
  else if Info.IsUser then
  begin
    ChatType := ctUser;
    var
      User: TVkProfile;
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
    var
      Group: TVkGroup;
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
          LabelInfo.Text := FMemberCount.ToString + ' ' +
            WordOfCount(FMemberCount, ['участник', 'участника', 'участников']);
      end;
    ctUser:
      begin
        if IsOnline then
          LabelInfo.Text := 'online'
        else if LastSeen <> 0 then
          LabelInfo.Text := WordOfSex(FUserSex, ['был', 'была']) + ' в сети ' + HumanDateTime(LastSeen, True)
        else
          LabelInfo.Text := '';
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
    LayoutMessageList.Width := VertScrollBoxMessages.ClientWidth - 1;
  LayoutMessageList.Position.Y := VertScrollBoxMessages.Height -
    LayoutMessageList.Height;
end;

procedure TFrameChat.CalcDisappear;
begin
  if Visible then
  begin
    var
      Content := VertScrollBoxMessages.Content.ScrollBox.ContentBounds;
    var
      Offset := Abs(VertScrollBoxMessages.ViewportPosition.Y - Content.Top);
    var
      ViewHeight := VertScrollBoxMessages.Height;
    var
      ContentHeight := LayoutMessageList.Height;

    var
      ATop := Offset;
    var
      ABottom := ATop + Min(ViewHeight, ContentHeight);

    for var Control in LayoutMessageList.Controls do
      if Control is TFrameMessage then
      begin
        var
          Vis :=((Control.BoundsRect.Bottom > ATop) and
          (Control.BoundsRect.Top < ABottom)) or
          ((Control.BoundsRect.Top < ABottom) and
          (Control.BoundsRect.Bottom > ATop));
        (Control as TFrameMessage).Visibility := Vis;
      end;
  end
  else
  begin
    for var Control in LayoutMessageList.Controls do
      if Control is TFrameMessage then
        (Control as TFrameMessage).Visibility := False;
  end;
end;

procedure TFrameChat.CircleToDownClick(Sender: TObject);
begin
  var
    Pos := VertScrollBoxMessages.Content.ScrollBox.ContentBounds.Bottom -
    VertScrollBoxMessages.Height;
  VertScrollBoxMessages.ViewportPosition :=
    TPointF.Create(0,
    Pos - Min(Abs(VertScrollBoxMessages.ViewportPosition.Y), 200));
  FChatScroll.ScrollDown;
end;

procedure TFrameChat.ShowHints;
begin
  if CircleToDown.Opacity = 0 then
  begin
    TAnimator.AnimateFloat(CircleToDown, 'Opacity', 1);
    CircleToDown.Fill.Color := $FF292929;
  end;
end;

procedure TFrameChat.HideHints;
begin
  TAnimator.AnimateFloat(CircleToDown, 'Opacity', 0);
end;

procedure TFrameChat.VertScrollBoxMessagesViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  CalcDisappear;
  var
    Content := VertScrollBoxMessages.Content.ScrollBox.ContentBounds;
  if Abs(NewViewportPosition.Y - Content.Top) < 500 then
    EndOfChat;
  if NewViewportPosition.Y = 0 then
    HideHints
  else
    ShowHints;
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
  Params.Fields([TVkExtendedField.OnlineInfo, TVkExtendedField.FirstNameAcc,
    TVkExtendedField.IsFriend, TVkExtendedField.CanSendFriendRequest,
    TVkExtendedField.CanWritePrivateMessage, TVkExtendedField.Verified,
    TVkExtendedField.Photo50]);
  if VK.Messages.GetConversationsById(Items, Params) then
  begin
    var
      Ext: IExtended := Items;
    if Length(Items.Items) > 0 then
      TThread.Synchronize(nil,
        procedure
        begin
          UpdateInfo(Items.Items[0], Ext);
          LayoutHead.Visible := True;
        end);
  end;
end;

procedure TFrameChat.MemoTextChangeTracking(Sender: TObject);
begin
  UpdateFooterSize;
  LabelTextHint.Visible := MemoText.Text.IsEmpty;
  ButtonAudio.Visible := MemoText.Text.IsEmpty;
  ButtonSend.Visible := not MemoText.Text.IsEmpty;
end;

procedure TFrameChat.MemoTextKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and not (ssCtrl in Shift) then
  begin
    Key := 0;
    TThread.ForceQueue(nil, SendMessage);
  end;
end;

procedure TFrameChat.PathCloseAddToFriendsClick(Sender: TObject);
begin
  IsNeedAddToFriends := False;
end;

procedure TFrameChat.PathUnpinMessageClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      try
        VK.Messages.Unpin(ConversationId);
      except
        TThread.Queue(nil,
          procedure
          begin
            ShowMessage('Не удалось открепить сообщение');
          end);
      end;
    end);
end;

procedure TFrameChat.UpdateFooterSize;
var
  H: Single;
begin
  H := Max(36, Min(MemoText.ContentSize.Height + 20, 220)) +
    RectangleMessage.Margins.Top + RectangleMessage.Margins.Bottom;
  if LayoutFooterBottom.Visible then
    H := H + LayoutFooterBottom.Height;
  if LayoutFooterTop.Visible then
    H := H + LayoutFooterTop.Height;
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
  LayoutMessageList.Visible := False;

  LabelTitle.Text := 'Загрузка...';
  LabelInfo.Text := '';
  LayoutHead.Visible := False;
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

procedure TFrameChat.SetHavePinned(const Value: Boolean);
begin
  FHavePinned := Value;
  if not FHavePinned then
    if Assigned(FPinnedMessage) then
    begin
      FPinnedMessage.Free;
      FPinnedMessage := nil;
      FPinnedMessageId := -1;
    end;
  RectanglePinnedMessage.Visible := FHavePinned;
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

procedure TFrameChat.SetIsCanPinMessage(const Value: Boolean);
begin
  FIsCanPinMessage := Value;
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
  var
    Reason: string := '';
  case FNotAllowedReason of
    TVkConversationDisableReason.UserBannedOrDeleted
    :
      Reason := 'Пользователь удалён.';
    TVkConversationDisableReason.UserBlacklisted:
      Reason :=
        'Нельзя отправить сообщение пользователю, который в чёрном списке';
    TVkConversationDisableReason.UserDisableGroupsMessages:
      Reason :=
        'Пользователь запретил сообщения от сообщества';
    TVkConversationDisableReason.UserPrivacy:
      Reason :=
        'Вы не можете отправить сообщение этому пользователю, поскольку он ограничил круг лиц, которые могут присылать ему сообщения.';
    TVkConversationDisableReason.GroupDisableMessages:
      Reason :=
        'Сообщество отключило сообщения.';
    TVkConversationDisableReason.GroupBannedMessages:
      Reason :=
        'В сообществе заблокированы сообщения';
    TVkConversationDisableReason.NoAccessChat:
      Reason :=
        'Вы были исключены из этого чата.';
    TVkConversationDisableReason.NoAccessEMail:
      Reason :=
        'Нет доступа к e-mail';
    TVkConversationDisableReason.NoAccessGroup:
      Reason :=
        'Вы не можете отправить сообщение этому сообществу, поскольку оно ограничило круг лиц, которые могут присылать ему сообщения.';
    TVkConversationDisableReason.Forbidden:
      Reason :=
        'Отправка сообщений ограничена';
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
    LabelAddFriendHint.Text := 'Добавьте ' + FUserAccFirstName +
      ' в друзья и общайтесь чаще';
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

procedure TFrameChat.SetIsUser(const Value: Boolean);
begin
  FIsUser := Value;
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
  ButtonBack.Repaint;
end;

procedure TFrameChat.SetOutRead(const Value: Int64);
begin
  FOutRead := Value;
end;

procedure TFrameChat.SetPinnedMessageId(const Value: Int64);
begin
  FPinnedMessageId := Value;
end;

procedure TFrameChat.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

procedure TFrameChat.SetUserSex(const Value: TVkSex);
begin
  FUserSex := Value;
  UpdateInfoText;
end;

procedure TFrameChat.SetVerified(const Value: Boolean);
begin
  FVerified := Value;
  LayoutVerified.Visible := FVerified;
end;

procedure TFrameChat.SetVisible(const Value: Boolean);
begin
  inherited;
  CalcDisappear;
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

{ TFrameMessagesHelper }

function TFrameMessagesHelper.ToIds: TArrayOfInteger;
begin
  SetLength(Result, Length(Self));
  for var i := Low(Self) to High(Self) do
    Result[i] := Self[i].MessageId;
end;

end.

