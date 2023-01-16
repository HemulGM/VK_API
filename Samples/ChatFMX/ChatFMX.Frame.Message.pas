unit ChatFMX.Frame.Message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, VK.Entity.Message, VK.Entity.PushSettings, VK.API,
  VK.Entity.Conversation, FMX.Memo.Style, System.Messaging, FMX.Ani,
  VK.Entity.Media, VK.Types, ChatFMX.Frame.Attachment.AudioMessage,
  ChatFMX.Frame.Attachment.Audio, ChatFMX.Frame.Attachment.Document,
  VK.Entity.Geo, ChatFMX.Frame.Attachment.Geo,
  ChatFMX.Frame.Attachment.ReplyMessage, VK.Entity.Common.ExtendedList,
  VK.Entity.Keyboard, ChatFMX.Classes, System.Threading;

type
  {$IFDEF DEBUG_ADAPTIVE}
    {$DEFINE ANDROID}
  {$ENDIF}

  TMessageSubType = (stNone, stGift, stMoneyTransfer, stMoneyRequest);

  TFrameMessage = class(TFrame)
    LayoutLeft: TLayout;
    RectangleBG: TRectangle;
    LayoutLeftTop: TLayout;
    CircleAvatar: TCircle;
    LayoutRight: TLayout;
    PathSelected: TPath;
    LayoutClient: TLayout;
    LayoutFrom: TLayout;
    LabelFrom: TLabel;
    LabelTime: TLabel;
    MemoText: TMemo;
    LayoutRightTop: TLayout;
    PathAnswer: TPath;
    LayoutAnswer: TLayout;
    LayoutFavorite: TLayout;
    PathStar: TPath;
    ColorAnimationAnswer: TColorAnimation;
    ColorAnimationStar: TColorAnimation;
    LayoutEdit: TLayout;
    PathEdit: TPath;
    ColorAnimationEdit: TColorAnimation;
    RectangleUnread: TRectangle;
    LayoutContent: TLayout;
    FlowLayoutMedia: TFlowLayout;
    LayoutSelectedIcon: TLayout;
    LayoutUpdated: TLayout;
    LabelUpdated: TLabel;
    LabelMessageType: TLabel;
    RectangleGiftBG: TRectangle;
    LayoutFwdMessages: TLayout;
    LayoutDeleted: TLayout;
    LabelMesDeleted: TLabel;
    LabelRestoreMessage: TLabel;
    procedure MemoTextChange(Sender: TObject);
    procedure MemoTextResize(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure LabelFromMouseEnter(Sender: TObject);
    procedure LabelFromMouseLeave(Sender: TObject);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoTextExit(Sender: TObject);
    procedure MemoTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FlowLayoutMediaResize(Sender: TObject);
    procedure LayoutFwdMessagesResize(Sender: TObject);
    procedure LabelRestoreMessageClick(Sender: TObject);
    procedure PathStarClick(Sender: TObject);
  private
    FVK: TCustomVK;
    FText: string;
    FFromText: string;
    FImageUrl: string;
    FImageFile: string;
    FMouseFrame: Boolean;
    FIsSelfMessage: Boolean;
    FIsCanEdit: Boolean;
    FIsUnread: Boolean;
    FIsImportant: Boolean;
    FIsSelected: Boolean;
    FWasSelectedText: Boolean;
    FOnSelectedChanged: TNotifyEvent;
    FDate: TDateTime;
    FUpdateTime: TDateTime;
    FIsGift: Boolean;
    FCanAnswer: Boolean;
    FFromSex: TVkSex;
    FVisibility: Boolean;
    FMessageSubType: TMessageSubType;
    FChatInfo: TChatInfo;
    FMessageId: Int64;
    FIsPinned: Boolean;
    FIsDeleted: Boolean;
    FConversationMessageId: Int64;
    procedure FOnAttachSelect(Sender: TObject);
    procedure SetText(const Value: string);
    procedure SetFromText(const Value: string);
    procedure SetImageUrl(const Value: string);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetMouseFrame(const Value: Boolean);
    procedure SetIsSelfMessage(const Value: Boolean);
    procedure SetIsCanEdit(const Value: Boolean);
    procedure SetIsUnread(const Value: Boolean);
    procedure SetIsImportant(const Value: Boolean);
    procedure SetIsSelected(const Value: Boolean);
    procedure SetOnSelectedChanged(const Value: TNotifyEvent);
    procedure CreatePhotos(Items: TVkAttachmentArray);
    procedure ClearMedia;
    procedure RecalcMedia;
    procedure CreateAutioMessages(Items: TVkAttachmentArray; Msg: TVkMessage);
    procedure CreateSticker(Items: TVkAttachmentArray);
    procedure CreateVideos(Items: TVkAttachmentArray);
    procedure SetDate(const Value: TDateTime);
    procedure CreateAudios(Items: TVkAttachmentArray);
    procedure CreateDocs(Items: TVkAttachmentArray);
    procedure CreateGeo(Value: TVkGeo);
    procedure SetUpdateTime(const Value: TDateTime);
    procedure CreateReplyMessage(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>);
    procedure SetIsGift(const Value: Boolean);
    procedure CreateGift(Items: TVkAttachmentArray; Msg: TVkMessage);
    procedure SetCanAnswer(const Value: Boolean);
    procedure CreateFwdMessages(Items: TArray<TVkMessage>; Data: TVkEntityExtendedList<TVkMessage>);
    procedure CreateLinks(Items: TVkAttachmentArray);
    procedure CreatePosts(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
    procedure SetFromSex(const Value: TVkSex);
    procedure CreateCalls(Items: TVkAttachmentArray);
    procedure SetVisibility(const Value: Boolean);
    procedure BroadcastVisible(const Value: Boolean);
    procedure CreateAlbums(Items: TVkAttachmentArray);
    procedure CreateMarket(Items: TVkAttachmentArray);
    procedure CreateMoney(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
    procedure SetMessageSubType(const Value: TMessageSubType);
    procedure UpdateSubType;
    procedure UpdateImportant;
    procedure FOnPhotosClick(Sender: TObject);
    function CollectPhotosFrom(const PhotoId: string; out Items: TArray<string>; out Index: Integer): Boolean;
    procedure CreateGraffiti(Items: TVkAttachmentArray);
    procedure CreatePoll(Items: TVkAttachmentArray);
    procedure SetChatInfo(const Value: TChatInfo);
    procedure CreateAudioPlaylists(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
    procedure CreateKeyborad(Keyboard: TVkKeyboard);
    procedure SetIsPinned(const Value: Boolean);
    procedure SetIsDeleted(const Value: Boolean);
    procedure CreateStories(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>; AChatInfo: TChatInfo);
    property Text: string read FText write SetText;
    property FromText: string read FFromText write SetFromText;
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property MouseFrame: Boolean read FMouseFrame write SetMouseFrame;
    property IsSelfMessage: Boolean read FIsSelfMessage write SetIsSelfMessage;
    property IsCanEdit: Boolean read FIsCanEdit write SetIsCanEdit;
    property IsUnread: Boolean read FIsUnread write SetIsUnread;
    property IsImportant: Boolean read FIsImportant write SetIsImportant;
    property IsSelected: Boolean read FIsSelected write SetIsSelected;
    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write SetOnSelectedChanged;
    property Date: TDateTime read FDate write SetDate;
    property UpdateTime: TDateTime read FUpdateTime write SetUpdateTime;
    property IsGift: Boolean read FIsGift write SetIsGift;
    property ChatCanAnswer: Boolean read FCanAnswer write SetCanAnswer;
    property FromSex: TVkSex read FFromSex write SetFromSex;
    property Visibility: Boolean read FVisibility write SetVisibility;
    property MessageSubType: TMessageSubType read FMessageSubType write SetMessageSubType;
    property ChatInfo: TChatInfo read FChatInfo write SetChatInfo;
    property IsPinned: Boolean read FIsPinned write SetIsPinned;
    property MessageId: Int64 read FMessageId;
    property ConversationMessageId: Int64 read FConversationMessageId;
    property IsDeleted: Boolean read FIsDeleted write SetIsDeleted;
    procedure UpdateFlags(ChangeType: TVkFlagsChangeType; Flags: TVkMessageFlags);
  end;

implementation

uses
  System.Math, System.DateUtils, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager, ChatFMX.Frame.Attachment.Photo, ChatFMX.Utils,
  ChatFMX.Frame.Attachment.Sticker, ChatFMX.Frame.Attachment.Video,
  ChatFMX.Frame.Attachment.Gift, ChatFMX.Frame.Attachment.Message,
  ChatFMX.Frame.Attachment.Link, ChatFMX.Frame.Attachment.Wall,
  ChatFMX.Frame.Attachment.Call, ChatFMX.Frame.Attachment,
  ChatFMX.Frame.Attachment.Album, ChatFMX.Frame.Attachment.Market,
  ChatFMX.Frame.Attachment.Money, ChatFMX.Frame.Window.Photo,
  ChatFMX.Frame.Attachment.Graffiti, ChatFMX.Frame.Attachment.Poll,
  ChatFMX.Frame.Attachment.AudioPlaylist, ChatFMX.Frame.Attachment.Keyboard,
  ChatFMX.Frame.Attachment.Story;

{$R *.fmx}

procedure TFrameMessage.BroadcastVisible(const Value: Boolean);
begin
  for var Control in LayoutClient.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
  for var Control in FlowLayoutMedia.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
  for var Control in LayoutFwdMessages.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
end;

procedure TFrameMessage.ClearMedia;
begin
  FlowLayoutMedia.BeginUpdate;
  try
    while FlowLayoutMedia.ControlsCount > 0 do
      FlowLayoutMedia.Controls[0].Free;
  finally
    FlowLayoutMedia.EndUpdate;
  end;
  RecalcMedia;
end;

procedure TFrameMessage.RecalcMedia;
begin
  var H: Single := 0;
  if ((FlowLayoutMedia.ControlsCount = 1) and (
    (FlowLayoutMedia.Controls[0] is TFrameAttachmentPhoto) or
    (FlowLayoutMedia.Controls[0] is TFrameAttachmentVideo))) then
  begin
    var Control := FlowLayoutMedia.Controls[0];
    var D := Control.Height / Control.Width;
    Control.Width := FlowLayoutMedia.Width;
    if (Control is TFrameAttachmentVideo) and (not (Control as TFrameAttachmentVideo).ShowCaption) then
    begin
      (Control as TFrameAttachmentVideo).ShowCaption := True;
      Control.Height := Control.Width * D +
        (Control as TFrameAttachmentVideo).LayoutCaption.Height;
    end
    else
      Control.Height := Control.Width * D;
    H := Max(Control.Position.Y + Control.Height, H);
  end
  else
    for var Control in FlowLayoutMedia.Controls do
    begin
      if Control.Width > FlowLayoutMedia.Width then
      begin
        var D := Control.Height / Control.Width;
        Control.Width := FlowLayoutMedia.Width;
        Control.Height := Control.Width * D;
      end;
      H := Max(Control.Position.Y + Control.Height, H);
    end;
  if FlowLayoutMedia.Height <> H then
  begin
    FlowLayoutMedia.Height := H;
    FrameResize(nil);
  end;
end;

constructor TFrameMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  MessageSubType := stNone;
  FVisibility := False;
  {$IFDEF ANDROID}
  LayoutSelectedIcon.Visible := False;
  LayoutLeft.Width := 51;
  LayoutRight.Visible := False;
  RectangleUnread.Margins.Right := 0;
  CircleAvatar.Margins.Right := 7;
  {$ENDIF}
  Name := '';
  FVK := AVK;
  RectangleBG.Visible := False;
  RectangleUnread.Visible := False;
  PathSelected.Visible := False;
  MemoText.DisableDisappear := True;
  MouseFrame := False;
  IsSelected := False;
  Text := '';
  IsGift := False;
  FIsDeleted := False;
  LayoutDeleted.Visible := False;
  LayoutFwdMessages.Visible := False;
  ClearMedia;
end;

destructor TFrameMessage.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameMessage.Fill(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>; AChatInfo: TChatInfo);
begin
  FMessageId := Item.Id;
  FConversationMessageId := Item.ConversationMessageId;
  IsPinned := Item.PinnedAt <> 0;
  ChatInfo := AChatInfo;
  ChatCanAnswer := ChatInfo.IsCanWrite;
  Text := Item.Text;
  Date := Item.Date;
  ImageUrl := '';
  IsSelfMessage := Item.FromId = FVK.UserId;
  IsCanEdit := HoursBetween(Now, Item.Date) < 24;
  IsImportant := Item.Important;
  IsUnread := False;
  UpdateTime := Item.UpdateTime;
  IsGift := False;

  var P2P := ChatInfo.IsP2P;
  IsUnread := Item.Id > ChatInfo.OutRead;
  if PeerIdIsUser(Item.FromId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.FromId, User) then
    begin
      if P2P then
        FromText := User.FirstName
      else
        FromText := User.FullName;
      FromSex := User.Sex;
      ImageUrl := User.Photo50;
    end;
  end
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.FromId, Group) then
    begin
      FromText := Group.Name;
      ImageUrl := Group.Photo50;
    end;
  end;

  if Assigned(Item.ReplyMessage) then
    CreateReplyMessage(Item.ReplyMessage, Data);

  if Length(Item.Attachments) > 0 then
  begin
    CreatePhotos(Item.Attachments);
    CreateVideos(Item.Attachments);
    CreateAudios(Item.Attachments);
    CreateAlbums(Item.Attachments);
    CreateDocs(Item.Attachments);
    CreateAutioMessages(Item.Attachments, Item);
    CreateSticker(Item.Attachments);
    CreateGift(Item.Attachments, Item);
    CreateLinks(Item.Attachments);
    CreatePosts(Item.Attachments, Data);
    CreateCalls(Item.Attachments);
    CreateMarket(Item.Attachments);
    CreateMoney(Item.Attachments, Data);
    CreateGraffiti(Item.Attachments);
    CreatePoll(Item.Attachments);
    CreateAudioPlaylists(Item.Attachments, Data);
    CreateStories(Item.Attachments, Data);
    RecalcMedia;
  end;

  if Assigned(Item.Geo) then
    CreateGeo(Item.Geo);

  if Length(Item.FwdMessages) > 0 then
    CreateFwdMessages(Item.FwdMessages, Data);

  if Assigned(Item.Keyboard) then
    CreateKeyborad(Item.Keyboard);

  RecalcSize;
end;

procedure TFrameMessage.CreateKeyborad(Keyboard: TVkKeyboard);
begin
  var Frame := TFrameAttachmentKeyboard.Create(LayoutClient, FVK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(Keyboard);
  Frame.PeerId := ChatInfo.PeerId;
end;

procedure TFrameMessage.CreatePosts(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Wall then
    begin
      var Frame := TFrameAttachmentWall.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Wall, Data);
    end;
end;

procedure TFrameMessage.CreateAudioPlaylists(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.AudioPlaylist then
    begin
      var Frame := TFrameAttachmentAudioPlaylist.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.AudioPlaylist, Data);
    end;
end;

procedure TFrameMessage.CreateStories(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Story then
    begin
      var Frame := TFrameAttachmentStory.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Story, Data);
    end;
end;

procedure TFrameMessage.CreateAlbums(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Album then
    begin
      var Frame := TFrameAttachmentAlbum.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Album);
    end;
end;

procedure TFrameMessage.CreateMarket(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Market then
    begin
      var Frame := TFrameAttachmentMarket.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Market);
    end;
end;

procedure TFrameMessage.CreateGraffiti(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Graffiti then
    begin
      var Frame := TFrameAttachmentGraffiti.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Graffiti);
    end;
end;

procedure TFrameMessage.CreatePoll(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Poll then
    begin
      var Frame := TFrameAttachmentPoll.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Poll);
    end;
end;

procedure TFrameMessage.CreateMoney(Items: TVkAttachmentArray; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
    if Item.&Type in [TVkAttachmentType.MoneyTransfer, TVkAttachmentType.MoneyRequest] then
    begin
      var Frame := TFrameAttachmentMoney.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      case Item.&Type of
        TVkAttachmentType.MoneyTransfer:
          begin
            Frame.Fill(Item.MoneyTransfer, Data);
            MessageSubType := stMoneyTransfer;
          end;
        TVkAttachmentType.MoneyRequest:
          begin
            Frame.Fill(Item.MoneyRequest, Data);
            MessageSubType := stMoneyRequest;
          end;
      end;
    end;
end;

procedure TFrameMessage.CreateFwdMessages(Items: TArray<TVkMessage>; Data: TVkEntityExtendedList<TVkMessage>);
begin
  for var Item in Items do
  begin
    var Frame := TFrameAttachmentMessage.Create(LayoutFwdMessages, FVK);
    Frame.OnSelect := FOnAttachSelect;
    Frame.Parent := LayoutFwdMessages;
    Frame.Position.Y := 10000;
    Frame.Align := TAlignLayout.Top;
    Frame.Fill(Item, Data, ChatInfo, True);
  end;
  LayoutFwdMessages.Visible := True;
  LayoutFwdMessages.Tag := 1;
  LayoutFwdMessages.RecalcSize;
end;

procedure TFrameMessage.CreateReplyMessage(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>);
begin
  var Frame := TFrameAttachmentReplyMessage.Create(LayoutClient, FVK);
  Frame.OnSelect := FOnAttachSelect;
  Frame.Parent := LayoutClient;
  Frame.Align := TAlignLayout.MostTop;
  Frame.Fill(Item, Data);
end;

procedure TFrameMessage.FlowLayoutMediaResize(Sender: TObject);
begin
  RecalcMedia;
end;

procedure TFrameMessage.CreateGeo(Value: TVkGeo);
begin
  var Frame := TFrameAttachmentGeo.Create(LayoutClient, FVK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(Value);
end;

function TFrameMessage.CollectPhotosFrom(const PhotoId: string; out Items: TArray<string>; out Index: Integer): Boolean;
begin
  SetLength(Items, FlowLayoutMedia.ControlsCount);
  Index := 0;
  if Length(Items) <= 0 then
    Exit(False);
  var i := 0;
  for var Control in FlowLayoutMedia.Controls do
    if Control is TFrameAttachmentPhoto then
    begin
      var Id :=(Control as TFrameAttachmentPhoto).Id;
      Items[i] := Id;
      if Id = PhotoId then
        Index := i;
      Inc(i);
    end;
  SetLength(Items, i);
  Result := True;
end;

procedure TFrameMessage.FOnPhotosClick(Sender: TObject);
var
  Frame: TFrameAttachmentPhoto absolute Sender;
begin
  if not (Sender is TFrameAttachmentPhoto) then
    Exit;

  var Items: TArray<string>;
  var CurrentIndex: Integer;
  if CollectPhotosFrom(Frame.Id, Items, CurrentIndex) then
  begin
    var Form := Application.MainForm;
    with TFrameWindowPhoto.Create(Form, FVK) do
    begin
      Parent := Form;
      Align := TAlignLayout.Contents;
      Fill(Items, CurrentIndex);
      ShowFrame;
    end;
  end;
end;

procedure TFrameMessage.CreatePhotos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
  begin
    if Item.&Type = TVkAttachmentType.Photo then
    begin
      var Frame := TFrameAttachmentPhoto.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
      Frame.OnClick := FOnPhotosClick;
      Frame.Fill(Item.Photo);
    end;
    if (Item.&Type = TVkAttachmentType.Doc) and (Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Doc, True);
    end;
  end;
end;

procedure TFrameMessage.CreateVideos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Video then
    begin
      var Frame := TFrameAttachmentVideo.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Video);
    end;
end;

procedure TFrameMessage.CreateGift(Items: TVkAttachmentArray; Msg: TVkMessage);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Gift then
    begin
      IsGift := True;
      var Frame := TFrameAttachmentGift.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Gift, Msg, ChatCanAnswer);
    end;
end;

procedure TFrameMessage.CreateSticker(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Sticker then
    begin
      var Frame := TFrameAttachmentSticker.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Sticker);
    end;
end;

procedure TFrameMessage.CreateAutioMessages(Items: TVkAttachmentArray; Msg: TVkMessage);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.AudioMessage then
    begin
      var Frame := TFrameAttachmentAudioMessage.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.AudioMessage, Msg.WasListened);
    end;
end;

procedure TFrameMessage.CreateAudios(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Audio then
    begin
      var Frame := TFrameAttachmentAudio.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Audio);
    end;
end;

procedure TFrameMessage.CreateLinks(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Link then
    begin
      var Frame := TFrameAttachmentLink.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Link);
    end;
end;

procedure TFrameMessage.CreateDocs(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if (Item.&Type = TVkAttachmentType.Doc) and (not Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Doc, False);
    end;
end;

procedure TFrameMessage.CreateCalls(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Call then
    begin
      var Frame := TFrameAttachmentCall.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Call);
    end;
end;

procedure TFrameMessage.FrameMouseEnter(Sender: TObject);
begin
  MouseFrame := True;
end;

procedure TFrameMessage.FrameMouseLeave(Sender: TObject);
begin
  MouseFrame := False;
end;

procedure TFrameMessage.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ANDROID}
  MemoText.SelLength := 0;
  IsSelected := not IsSelected;
  {$ENDIF}
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  var H: Single := LayoutContent.Padding.Top + LayoutContent.Padding.Bottom;
  RecalcMedia;
  for var Control in LayoutClient.Controls do
    if Control.IsVisible then
      H := H + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  H := Floor(Max(H, 52));
  if Height <> H then
  begin
    Height := H;
    if Assigned(ParentControl) then
      ParentControl.RecalcSize;
  end;
end;

procedure TFrameMessage.LabelFromMouseEnter(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameMessage.LabelFromMouseLeave(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameMessage.LabelRestoreMessageClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      FVK.Messages.Restore(MessageId);
    end);
end;

procedure TFrameMessage.LayoutFwdMessagesResize(Sender: TObject);
begin
  var H: Single := 0;
  for var Control in LayoutFwdMessages.Controls do
    H := Max(Control.Position.Y + Control.Height, H);
  LayoutFwdMessages.Height := H;
  FrameResize(nil);
end;

procedure TFrameMessage.MemoTextChange(Sender: TObject);
begin
  MemoText.Height := MemoText.ContentSize.Size.Height + 1;
  FrameResize(nil);
end;

procedure TFrameMessage.MemoTextExit(Sender: TObject);
begin
  MemoText.SelLength := 0;
end;

procedure TFrameMessage.MemoTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FWasSelectedText := MemoText.SelLength > 0;
end;

procedure TFrameMessage.MemoTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (MemoText.SelLength > 0) or FWasSelectedText then
    Exit;
  {$IFNDEF ANDROID}
  //IsSelected := not IsSelected;
  FrameMouseUp(Sender, Button, Shift, X, Y);
  {$ENDIF}
end;

procedure TFrameMessage.MemoTextResize(Sender: TObject);
begin
  MemoTextChange(Sender);
end;

procedure TFrameMessage.PathStarClick(Sender: TObject);
begin
  var MsgId: Int64;
  var Mark: Boolean := not IsImportant;
  TTask.Run(
    procedure
    begin
      FVK.Messages.MarkAsImportant(MsgId, MessageId, Mark);
    end);
end;

procedure TFrameMessage.SetCanAnswer(const Value: Boolean);
begin
  FCanAnswer := Value;
end;

procedure TFrameMessage.SetChatInfo(const Value: TChatInfo);
begin
  FChatInfo := Value;
end;

procedure TFrameMessage.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  LabelTime.Text := FormatDateTime('HH:nn', FDate);
  LabelTime.Hint := FormatDateTime('c', FDate);
end;

procedure TFrameMessage.SetFromSex(const Value: TVkSex);
begin
  FFromSex := Value;
  UpdateSubType;
end;

procedure TFrameMessage.SetFromText(const Value: string);
begin
  FFromText := Value;
  LabelFrom.Text := FFromText;
end;

procedure TFrameMessage.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameMessage.SetIsCanEdit(const Value: Boolean);
begin
  FIsCanEdit := Value;
end;

procedure TFrameMessage.SetIsDeleted(const Value: Boolean);
begin
  FIsDeleted := Value;
  LayoutDeleted.Visible := FIsDeleted;
  LabelMesDeleted.Text := 'Сообщение удалено.';
  if FIsDeleted then
  begin
    IsSelected := False;
    MouseFrame := False;
  end;
  for var Control in LayoutClient.Controls do
    if (Control <> LayoutFrom) and (Control <> LayoutDeleted) then
      if FIsDeleted then
      begin
        if Control.Visible then
          Control.Tag := 1;
        Control.Visible := False;
      end
      else
      begin
        Control.Visible := Control.Tag = 1;
      end;
  RecalcSize;
end;

procedure TFrameMessage.SetIsGift(const Value: Boolean);
begin
  FIsGift := Value;
  MemoText.Visible := not FIsGift;
  if FIsGift then
  begin
    MessageSubType := stGift;
    PathSelected.Fill.Color := $FFe3d3ac;
    LabelFrom.FontColor := TAlphaColorRec.White;
    LabelTime.FontColor := $FFE3D3AC;

    PathAnswer.Fill.Color := $99FFFFFF;
    ColorAnimationAnswer.StartValue := $99FFFFFF;
    ColorAnimationAnswer.StopValue := $EEFFFFFF;

    PathEdit.Fill.Color := $99FFFFFF;
    ColorAnimationEdit.StartValue := $99FFFFFF;
    ColorAnimationEdit.StopValue := $EEFFFFFF;
  end
  else
  begin
    PathSelected.Fill.Color := $FF71AAEB;
    LabelFrom.FontColor := $FF71AAEB;
    LabelTime.FontColor := $FF828282;

    PathAnswer.Fill.Color := $FF5B5B5B;
    ColorAnimationAnswer.StartValue := $FF5B5B5B;
    ColorAnimationAnswer.StopValue := $FF6A6A6A;

    PathEdit.Fill.Color := $FF5B5B5B;
    ColorAnimationEdit.StartValue := $FF5B5B5B;
    ColorAnimationEdit.StopValue := $FF6A6A6A;
  end;
  RectangleGiftBG.Visible := FIsGift;
  UpdateImportant;
end;

procedure TFrameMessage.UpdateFlags(ChangeType: TVkFlagsChangeType; Flags: TVkMessageFlags);
begin
  case ChangeType of
    TVkFlagsChangeType.Replace:
      begin
        if (TVkMessageFlag.Deleted in Flags) or (TVkMessageFlag.DeleteForAll in Flags) then
          IsDeleted := True;
        if TVkMessageFlag.Spam in Flags then
        begin
          IsDeleted := True;
          LabelMesDeleted.Text := 'Сообщение помечено как спам и удалено.';
        end;
        if TVkMessageFlag.Important in Flags then
          IsImportant := True;
        if TVkMessageFlag.Unread in Flags then
          IsUnread := True;
      end;
    TVkFlagsChangeType.set:
      begin
        if (TVkMessageFlag.Deleted in Flags) or (TVkMessageFlag.DeleteForAll in Flags) then
          IsDeleted := True;
        if TVkMessageFlag.Spam in Flags then
        begin
          IsDeleted := True;
          LabelMesDeleted.Text := 'Сообщение помечено как спам и удалено.';
        end;
        if TVkMessageFlag.Important in Flags then
          IsImportant := True;
        if TVkMessageFlag.Unread in Flags then
          IsUnread := True;
      end;
    TVkFlagsChangeType.Reset:
      begin
        if (TVkMessageFlag.Deleted in Flags) or (TVkMessageFlag.DeleteForAll in Flags) then
          IsDeleted := False;
        if TVkMessageFlag.Spam in Flags then
          IsDeleted := False;
        if TVkMessageFlag.Important in Flags then
          IsImportant := False;
        if TVkMessageFlag.Unread in Flags then
          IsUnread := False;
      end;
  end;
end;

procedure TFrameMessage.UpdateImportant;
begin
  if FIsImportant then
  begin
    if FIsGift then
    begin
      PathStar.Fill.Color := $FF5B5B5B;
      ColorAnimationStar.StartValue := $FF5B5B5B;
      ColorAnimationStar.StopValue := $FF6A6A6A;
    end
    else
    begin
      ColorAnimationStar.StartValue := $FF71AAEB;
      ColorAnimationStar.StopValue := $FF71AAEB;
      PathStar.Fill.Color := $FF71AAEB;
    end;
  end
  else
  begin
    if FIsGift then
    begin
      PathStar.Fill.Color := $99FFFFFF;
      ColorAnimationStar.StartValue := $99FFFFFF;
      ColorAnimationStar.StopValue := $EEFFFFFF;
    end
    else
    begin
      ColorAnimationStar.StartValue := $FF5B5B5B;
      ColorAnimationStar.StopValue := $FF6A6A6A;
      PathStar.Fill.Color := $FF5B5B5B;
    end;
  end;
  LayoutFavorite.Visible := FIsImportant;
end;

procedure TFrameMessage.SetIsImportant(const Value: Boolean);
begin
  FIsImportant := Value;
  UpdateImportant;
end;

procedure TFrameMessage.SetIsPinned(const Value: Boolean);
begin
  FIsPinned := Value;
end;

procedure TFrameMessage.SetIsSelected(const Value: Boolean);
var
  Changed: Boolean;
begin
  if IsDeleted and Value then
    Exit;
  Changed := FIsSelected <> Value;
  FIsSelected := Value;
  if FIsSelected then
  begin
    MemoText.SetFocus;
    PathSelected.Opacity := 1;
  end
  else
    PathSelected.Opacity := 0.7;
  RectangleBG.Visible := FIsSelected;
  SetMouseFrame(FMouseFrame);
  if not IsUpdating then
    if Changed and Assigned(FOnSelectedChanged) then
      FOnSelectedChanged(Self);
end;

procedure TFrameMessage.SetIsSelfMessage(const Value: Boolean);
begin
  FIsSelfMessage := Value;
end;

procedure TFrameMessage.SetIsUnread(const Value: Boolean);
begin
  FIsUnread := Value;
  RectangleUnread.Visible := FIsUnread;
end;

procedure TFrameMessage.UpdateSubType;
begin
  LabelMessageType.Visible := MessageSubType <> stNone;
  case MessageSubType of
    stGift:
      begin
        LabelMessageType.Text := WordOfSex(FFromSex, ['отправил', 'отправила']) + ' подарок';
        LabelMessageType.FontColor := $FFFFFFFF;
      end;
    stMoneyTransfer:
      begin
        LabelMessageType.Text := WordOfSex(FFromSex, ['перевёл', 'перевела']) + ' деньги';
        LabelMessageType.FontColor := $FF939393;
      end;
    stMoneyRequest:
      begin
        LabelMessageType.Text := WordOfSex(FFromSex, ['отправил', 'отправила']) + ' запрос на перевод';
        LabelMessageType.FontColor := $FF939393;
      end;
  end;
end;

procedure TFrameMessage.SetMessageSubType(const Value: TMessageSubType);
begin
  FMessageSubType := Value;
  UpdateSubType;
end;

procedure TFrameMessage.SetMouseFrame(const Value: Boolean);
begin
  if IsDeleted and Value then
    Exit;
  FMouseFrame := Value;
  PathSelected.Visible := FMouseFrame or IsSelected;
  LayoutAnswer.Visible := (not IsSelfMessage) and FMouseFrame and ChatCanAnswer;
  LayoutEdit.Visible := (IsSelfMessage and IsCanEdit) and FMouseFrame;
  if not IsImportant then
    LayoutFavorite.Visible := FMouseFrame;
end;

procedure TFrameMessage.SetOnSelectedChanged(const Value: TNotifyEvent);
begin
  FOnSelectedChanged := Value;
end;

procedure TFrameMessage.FOnAttachSelect(Sender: TObject);
begin
  IsSelected := not IsSelected;
end;

procedure TFrameMessage.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    CircleAvatar.Fill.Kind := TBrushKind.Solid
  else
  try
    CircleAvatar.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  FText := ParseMention(Value);
  MemoText.Visible := not FText.IsEmpty;
  MemoText.Text := FText;
  if not FText.IsEmpty then
  begin
    (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
    (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  end;
end;

procedure TFrameMessage.SetUpdateTime(const Value: TDateTime);
begin
  FUpdateTime := Value;
  if FUpdateTime > 0 then
  begin
    LayoutUpdated.Visible := True;
    LabelUpdated.Hint := 'изменено ' + HumanDateTime(FUpdateTime, True);
  end
  else
    LayoutUpdated.Visible := False;
end;

procedure TFrameMessage.SetVisibility(const Value: Boolean);
begin
  if FVisibility = Value then
    Exit;
  FVisibility := Value;
  if FVisibility then
    Opacity := 1
  else
    Opacity := 0;
  BroadcastVisible(FVisibility);
end;

end.

