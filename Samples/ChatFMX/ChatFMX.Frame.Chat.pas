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

  TFrameChat = class(TFrame)
    ListBoxChat: TListBox;
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
    ListBoxItem2: TListBoxItem;
    ListBoxItem1: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem3: TListBoxItem;
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
    procedure ListBoxChatViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure VertScrollBoxMessagesResize(Sender: TObject);
    procedure LayoutMessageListResize(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
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
    procedure AddMessageObject(Item: TControl);
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
  end;

implementation

uses
  System.Threading, VK.Messages, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager, System.Math;

{$R *.fmx}

{ TFrameChat }

procedure TFrameChat.AddMessageObject(Item: TControl);
var
  IsEnd: Boolean;
begin
  IsEnd := (VertScrollBoxMessages.ViewportPosition.Y + VertScrollBoxMessages.Height) - VertScrollBoxMessages.ContentBounds.Bottom < 10;

  //LayoutMessageList.InsertComponent(Item);
  Item.Align := TAlignLayout.Bottom;
  Item.Position.Y := LayoutMessageList.Height + 10;
  //Item.Parent := LayoutMessageList;

  LayoutMessageList.RecalcSize;

  if IsEnd then
    VertScrollBoxMessages.ViewportPosition := TPointF.Create(0, VertScrollBoxMessages.ContentBounds.Height);
end;

procedure TFrameChat.ButtonSearchClick(Sender: TObject);
begin
  var Item := TFrameMessage.Create(nil, FVK);
  //Item.Text := 'test ' + LayoutMessageList.ControlsCount.ToString;
  //Item.Height := Random(150) + 20;
  AddMessageObject(Item);
end;

constructor TFrameChat.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FLoading := True;
  FVK := AVK;
  Name := '';
  ListBoxChat.AniCalculations.Animation := True;
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

procedure TFrameChat.CreateMessageItem(const Item: TVkMessage; Data: TVkMessageHistory);
var
  IsEnd: Boolean;
begin
  IsEnd := (VertScrollBoxMessages.ViewportPosition.Y + VertScrollBoxMessages.Height) - VertScrollBoxMessages.ContentBounds.Bottom < 10;

  var MessageItem := TFrameMessage.Create(LayoutMessageList, FVK);  
  MessageItem.Parent := LayoutMessageList;
  MessageItem.Position.Y := 0;
  MessageItem.Fill(Item, Data);   
  MessageItem.Align := TAlignLayout.Bottom;

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
        LayoutMessageList.Visible := True;
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

  if Assigned(Info.PushSettings) then
  begin
    IsMuted := Info.PushSettings.NoSound;
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
  case FChatType of
    ctChat:
      begin
        LabelInfo.Text := FMemberCount.ToString + ' участников';
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
end;

procedure TFrameChat.VertScrollBoxMessagesResize(Sender: TObject);
begin
  LayoutMessageList.Width := VertScrollBoxMessages.Width;
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
          LayoutHead.Visible := True;
        end);
    end;
  finally
    Items.Free;
  end;
end;

procedure TFrameChat.ReloadAsync;
begin
  ListBoxChat.Clear;
  LayoutMessageList.BeginUpdate;
  try
    while LayoutMessageList.ControlsCount > 0 do
      LayoutMessageList.Controls[0].Free;
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

procedure TFrameChat.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    CircleImage.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameChat.SetIsMobile(const Value: Boolean);
begin
  FIsMobile := Value;
  LayoutMobileIndicate.Visible := FIsMobile;
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

end.

