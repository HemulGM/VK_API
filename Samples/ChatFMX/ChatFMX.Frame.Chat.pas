unit ChatFMX.Frame.Chat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Controls.Presentation,
  ChatFMX.DM.Res, FMX.Edit, VK.Types, VK.API, FMX.ImgList, VK.Entity.Message,
  VK.Entity.Conversation, System.Messaging, ChatFMX.Frame.Message;

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
    Button1: TButton;
    EditMessage: TEdit;
    Button2: TButton;
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
    procedure ListBoxChatViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure VertScrollBoxMessagesResize(Sender: TObject);
    procedure LayoutMessageListResize(Sender: TObject);
    procedure LayoutUnselClick(Sender: TObject);
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
    procedure SetConversationId(const Value: TVkPeerId);
    procedure ReloadAsync;
    procedure SetVK(const Value: TCustomVK);
    procedure EndOfChat;
    procedure LoadConversationAsync;
    procedure LoadConversationInfoAsync;
    procedure CreateMessageItem(const Item: TVkMessage; Data: TVkMessageHistory);
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
  end;

implementation

uses
  System.Threading, VK.Messages, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager, System.Math, FMX.Ani, ChatFMX.Utils;

{$R *.fmx}

{ TFrameChat }

constructor TFrameChat.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FLoading := True;
  FVK := AVK;
  Name := '';
  HeadMode := hmNormal;
end;

procedure TFrameChat.EndOfChat;
begin
  if FLoading then
    Exit;
  if not FOffsetEnd then
  begin
    Inc(FOffset, 20);
    TTask.Run(LoadConversationAsync);
  end;
end;

procedure TFrameChat.LayoutMessageListResize(Sender: TObject);
begin
  var Sz: Single := 0;
  for var Control in LayoutMessageList.Controls do
    Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  LayoutMessageList.Height := Sz + 10;
end;

procedure TFrameChat.LayoutUnselClick(Sender: TObject);
begin
  UnselectAll;
end;

procedure TFrameChat.ListBoxChatViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin     {
  if FLoading then
    Exit;
  if NewViewportPosition.Y = 0 then
    Exit;
  if NewViewportPosition.Y + ListBoxChat.Height >= ListBoxChat.ContentBounds.Height then
    EndOfChat; }
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

procedure TFrameChat.CreateMessageItem(const Item: TVkMessage; Data: TVkMessageHistory);
var
  IsEnd: Boolean;
begin
  IsEnd := (VertScrollBoxMessages.ViewportPosition.Y + VertScrollBoxMessages.Height) - VertScrollBoxMessages.ContentBounds.Bottom < 10;

  var MessageItem := TFrameMessage.Create(LayoutMessageList, FVK);
  MessageItem.Parent := LayoutMessageList;
  MessageItem.Position.Y := -1000;
  MessageItem.Fill(Item, Data);
  MessageItem.Align := TAlignLayout.Bottom;
  MessageItem.OnSelectedChanged := FOnMessageSelected;

  if IsEnd then
    VertScrollBoxMessages.ViewportPosition := TPointF.Create(0, VertScrollBoxMessages.ContentBounds.Height);
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
  Params.Fields([TVkProfileField.Photo50]);
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
        LayoutMessageList.RecalcSize;
        VertScrollBoxMessagesResize(nil);
        LayoutMessageList.Opacity := 1;
        //TAnimator.AnimateFloat(LayoutMessageList, 'Opacity', 1);
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
    var UserId := FindUser(Info.Peer.Id, Data.Profiles);
    if UserId >= 0 then
    begin
      var User := Data.Profiles[UserId];
      Title := User.FullName;
      ImageUrl := User.Photo50;
      if Assigned(User.OnlineInfo) then
      begin
        LastSeen := User.OnlineInfo.LastSeen;
        IsOnline := User.OnlineInfo.IsOnline;
        IsMobile := User.OnlineInfo.IsMobile;
      end;
      //CanCall := User.
    end;
  end
  else if Info.IsGroup then
  begin
    ChatType := ctGroup;
    var GroupId := FindGroup(Info.Peer.Id, Data.Groups);
    if GroupId >= 0 then
    begin
      Title := Data.Groups[GroupId].Name;
      ImageUrl := Data.Groups[GroupId].Photo50;
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
        else
          LabelInfo.Text := 'был в сети ' + FormatDateTime('d mmm', LastSeen);
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
    LayoutMessageList.Width := VertScrollBoxMessages.Width
  else
    for var Control in LayoutMessageList.Controls do
      Control.RecalcSize;
  LayoutMessageList.Position.Y := VertScrollBoxMessages.Height - LayoutMessageList.Height;
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
  if VK.Messages.GetConversationsById(Items, Params) then
  try
    if Length(Items.Items) > 0 then
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          UpdateInfo(Items.Items[0], Items);
          LayoutHead.Opacity := 1;
          //TAnimator.AnimateFloat(LayoutHead, 'Opacity', 1);
        end);
    end;
  finally
    Items.Free;
  end;
end;

procedure TFrameChat.ReloadAsync;
begin
  LayoutMessageList.BeginUpdate;
  try
    while LayoutMessageList.ControlsCount > 0 do
      LayoutMessageList.Controls[0].Free;
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

procedure TFrameChat.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
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

