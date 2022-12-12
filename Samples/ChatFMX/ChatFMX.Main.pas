unit ChatFMX.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, ChatFMX.Frame.Chat, FMX.Controls.Presentation,
  FMX.Edit, FMX.StdCtrls, System.ImageList, FMX.ImgList, FMX.SVGIconImageList,
  FMX.Effects, FMX.Filter.Effects, ChatFMX.DM.Res, VK.API, VK.Components,
  VK.Entity.Conversation, System.Messaging, VK.Types,
  System.Generics.Collections, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  ChatFMX.Frame.Loading;

type
  TChats = class(TList<TFrameChat>)
    function FindChat(const PeerId: TVkPeerId; var Frame: TFrameChat): Boolean;
  end;

  TListBoxLoading = class(TListBoxItem)
  end;

  TFormMain = class(TForm)
    LayoutClient: TLayout;
    HorzScrollBoxContent: THorzScrollBox;
    StyleBook: TStyleBook;
    Rectangle1: TRectangle;
    VK: TVK;
    LayoutChat: TLayout;
    Line1: TLine;
    LayoutNoChat: TLayout;
    Layout2: TLayout;
    Path1: TPath;
    Text1: TText;
    LayoutChatFrames: TLayout;
    LayoutChats: TLayout;
    ListBoxChats: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    RectangleHead: TRectangle;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RectangleFooter: TRectangle;
    LabelChatsMode: TLabel;
    LayoutHeader: TLayout;
    RectangleHeader: TRectangle;
    CircleAvatar: TCircle;
    LayoutLoading: TLayout;
    RectangleLoadBG: TRectangle;
    PathSplashLogo: TPath;
    LabelLogoSubTitle: TLabel;
    LayoutError: TLayout;
    Rectangle2: TRectangle;
    Label2: TLabel;
    ButtonRelogin: TButton;
    PanelLoader: TPanel;
    LayoutLogo: TLayout;
    Path3: TPath;
    Path4: TPath;
    Path5: TPath;
    LayoutSplash: TLayout;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    Layout1: TLayout;
    ScrollBarChats: TSmallScrollBar;
    LayoutChatsLoading: TLayout;
    LayoutChatLoadingAni: TLayout;
    FrameLoading1: TFrameLoading;
    Memo1: TMemo;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VKAuth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
    procedure VKLogin(Sender: TObject);
    procedure LabelChatsModeClick(Sender: TObject);
    procedure ListBoxChatsViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonReloginClick(Sender: TObject);
    procedure VKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure ListBoxChatsApplyStyleLookup(Sender: TObject);
    procedure Path3Click(Sender: TObject);
    procedure VKLog(Sender: TObject; const Value: string);
  private
    FToken: string;
    FChangePasswordHash: string;
    FTokenExpiry: Int64;
    FUnreadOnly: Boolean;
    FLoading: Boolean;
    FListChatsOffset: Integer;
    FListChatsOffsetEnd: Boolean;
    FChats: TChats;
    FLoadingItem: TListBoxLoading;
    procedure FOnReadyAvatar(const Sender: TObject; const M: TMessage);
    procedure FOnChatItemClick(Sender: TObject);
    procedure LoadConversationsAsync;
    procedure CreateChatItem(Chat: TVkConversationItem; Data: TVkConversationItems);
    procedure SetUnreadOnly(const Value: Boolean);
    procedure FOnLog(Sender: TObject; Value: string);
    procedure EndOfChats;
    procedure Reload;
    procedure LoadChat(PeerId: TVkPeerId);
    function CreateChat(PeerId: TVkPeerId): TFrameChat;
    procedure ShowChat(Frame: TFrameChat);
    procedure Login;
    procedure LoadDone;
    procedure DoErrorLogin;
    procedure CreateLoadingItem;
    procedure ClearChatList;
  public
    property UnreadOnly: Boolean read FUnreadOnly write SetUnreadOnly;
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Math, System.Threading, VK.FMX.OAuth2, System.IOUtils, VK.Clients,
  ChatFMX.View.ChatItem, VK.Messages, ChatFMX.PreviewManager, FMX.Ani,
  HGM.FMX.SmoothScroll;

{$R *.fmx}

procedure TFormMain.FOnChatItemClick(Sender: TObject);
var
  Item: TListBoxItemChat absolute Sender;
begin
  Item.IsSelected := True;
  LoadChat(Item.ConversationId);
end;

procedure TFormMain.FOnLog(Sender: TObject; Value: string);
begin
  //FrameChat1.ListBoxChat.Items.Add(Value);
end;

procedure TFormMain.FOnReadyAvatar(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> VK.UserPhoto100 then
    Exit;
  try
    CircleAvatar.Fill.Bitmap.Bitmap.LoadFromFile(Data.Value.FileName);
    CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
  TPreview.Instance.Unsubscribe(FOnReadyAvatar);
end;

procedure TFormMain.Login;
begin
  try
    VK.Login;
  finally
    if not VK.IsLogin then
      TThread.Queue(nil, DoErrorLogin);
  end;
end;

procedure TFormMain.Path3Click(Sender: TObject);
begin
  Memo1.Visible := not Memo1.Visible;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FLoading := True;
  FLoadingItem := nil;
  LayoutLoading.Visible := True;
  HorzScrollBoxContent.Visible := False;
  PanelLoader.StylesData['left.Enabled'] := True;
  PanelLoader.Visible := True;
  FChats := TChats.Create;
  LayoutError.Visible := False;
  ListBoxChats.AniCalculations.Animation := True;
  TPreview.Instance.OnLog := FOnLog;
  VK.Application := TVkApplicationData.VKAdmin;
  if TFile.Exists('token.tmp') then
    VK.Token := TFile.ReadAllText('token.tmp');
  TTask.Run(Login);
  //LoadDone;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TPreview.Instance.Unsubscribe(FOnReadyAvatar);
  FChats.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LayoutClient.Width := Max(Min(1000, ClientWidth), 800) - 40;
end;

procedure TFormMain.CreateChatItem(Chat: TVkConversationItem; Data: TVkConversationItems);
var
  ListItem: TListBoxItemChat;
begin
  ListItem := TListBoxItemChat.Create(ListBoxChats, VK);
  ListItem.Height := 63;
  ListBoxChats.AddObject(ListItem);
  ListItem.Fill(Chat, Data);
  ListItem.OnClick := FOnChatItemClick;
  //ListItem.ApplyStyle;
end;

procedure TFormMain.LabelChatsModeClick(Sender: TObject);
begin
  if FLoading then
    Exit;
  FLoading := True;
  UnreadOnly := not UnreadOnly;
  TTask.Run(LoadConversationsAsync);
end;

procedure TFormMain.EndOfChats;
begin
  if FLoading then
    Exit;
  FLoading := True;
  if not FListChatsOffsetEnd then
  begin
    Inc(FListChatsOffset, 20);
    TTask.Run(LoadConversationsAsync);
  end;
end;

procedure TFormMain.ListBoxChatsApplyStyleLookup(Sender: TObject);
var
  Scroll: TSmallScrollBar;
  Layout: TLayout;
begin
  if ListBoxChats.FindStyleResource('small_scroll', Layout) then
  begin
    Layout.BringToFront;
  end;
  if ListBoxChats.FindStyleResource('vscrollbar', Scroll) then
  begin
    Scroll.BringToFront;
    Scroll.HitTest := True;
  end;
end;

procedure TFormMain.ListBoxChatsViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  //
  if Assigned(FLoadingItem) then
    LayoutChatLoadingAni.Position.Y := -(NewViewportPosition.Y - FLoadingItem.Position.Y)
  else
    LayoutChatLoadingAni.Position.Y := -LayoutChatLoadingAni.Height;
  if FLoading then
    Exit;
  if NewViewportPosition.Y = 0 then
    Exit;
  if NewViewportPosition.Y + ListBoxChats.Height >= (ListBoxChats.ContentBounds.Height - ListBoxChats.Height) then
    EndOfChats;
end;

procedure TFormMain.ButtonReloginClick(Sender: TObject);
begin
  PanelLoader.Visible := True;
  LayoutError.Visible := False;
  TTask.Run(
    procedure
    begin
      Sleep(2000);
      Login;
    end);
end;

function TFormMain.CreateChat(PeerId: TVkPeerId): TFrameChat;
begin
  Result := TFrameChat.Create(Self, VK);
  Result.Visible := False;
  Result.Parent := LayoutChatFrames;
  Result.Align := TAlignLayout.Client;
  Result.Load(PeerId);
  FChats.Add(Result);
end;

procedure TFormMain.ShowChat(Frame: TFrameChat);
begin
  for var Control in LayoutChatFrames.Controls do
    Control.Visible := Control = Frame;
end;

procedure TFormMain.LoadChat(PeerId: TVkPeerId);
var
  Frame: TFrameChat;
begin
  if not FChats.FindChat(PeerId, Frame) then
    Frame := CreateChat(PeerId);
  ShowChat(Frame);
end;

procedure TFormMain.LoadConversationsAsync;
var
  Items: TVkConversationItems;
  Params: TVkParamsConversationsGet;
begin
  Params.Extended;
  Params.Offset(FListChatsOffset);
  Params.Count(20);
  Params.Fields([TVkProfileField.Photo50, TVkProfileField.Verified], [TVkGroupField.Verified]);
  if FUnreadOnly then
    Params.Filter(TVkConversationFilter.Unread);
  try
    if VK.Messages.GetConversations(Items, Params) then
    try
      if Length(Items.Items) < 20 then
        FListChatsOffsetEnd := True;
      TThread.Synchronize(nil,
        procedure
        begin
          ListBoxChats.BeginUpdate;
          try
            for var Item in Items.Items do
              CreateChatItem(Item, Items);
            if FListChatsOffsetEnd then
            begin
              LayoutChatLoadingAni.Position.Y := -LayoutChatLoadingAni.Height;
              FLoadingItem.Free;
              FLoadingItem := nil;
            end
            else if Assigned(FLoadingItem) then
              FLoadingItem.Index := ListBoxChats.Count;
          finally
            ListBoxChats.EndUpdate;
          end;
          FLoading := False;
        end);
    finally
      Items.Free;
    end;
  except
    Dec(FListChatsOffset);
    FLoading := False;
  end;
end;

procedure TFormMain.CreateLoadingItem;
begin
  FLoadingItem := TListBoxLoading.Create(ListBoxChats);
  FLoadingItem.Text := '';
  FLoadingItem.DisableDisappear := True;
  FLoadingItem.StyleLookup := 'item_loading';
  FLoadingItem.Height := 40;
  ListBoxChats.AddObject(FLoadingItem);
end;

procedure TFormMain.ClearChatList;
begin
  ListBoxChats.Clear;
  CreateLoadingItem;
end;

procedure TFormMain.SetUnreadOnly(const Value: Boolean);
begin
  FUnreadOnly := Value;
  FListChatsOffset := 0;
  FListChatsOffsetEnd := False;
  ClearChatList;
  case FUnreadOnly of
    True:
      LabelChatsMode.Text := 'Показать все';
    False:
      LabelChatsMode.Text := 'Показать непрочитанные';
  end;
end;

procedure TFormMain.VKAuth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
var
  AToken: string;
  ATokenExpiry: Int64;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if FToken.IsEmpty then
      begin
        TFormFMXOAuth2.Execute(Url,
          procedure(Form: TFormFMXOAuth2)
          begin
            FToken := Form.Token;
            FTokenExpiry := Form.TokenExpiry;
            FChangePasswordHash := Form.ChangePasswordHash;
            if not FToken.IsEmpty then
              VK.Login
            else
            begin
              DoErrorLogin;
            end;
          end);
      end
      else
      begin
        AToken := FToken;
        ATokenExpiry := FTokenExpiry;
      end;
    end);
  Token := AToken;
  TokenExpiry := ATokenExpiry;
end;

procedure TFormMain.DoErrorLogin;
begin
  FToken := '';
  VK.Token := '';
  LayoutLoading.Opacity := 1;
  LayoutLoading.Visible := True;
  LayoutError.Visible := True;
  PanelLoader.Visible := False;
  HorzScrollBoxContent.Visible := False;
end;

procedure TFormMain.VKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if not VK.IsLogin then
    DoErrorLogin;
end;

procedure TFormMain.LoadDone;
begin
  TAnimator.AnimateFloatWait(LayoutLoading, 'Opacity', 0);
  LayoutLoading.Visible := False;
  HorzScrollBoxContent.Visible := True;
end;

procedure TFormMain.Reload;
begin
  ClearChatList;
  FListChatsOffset := 0;
  FListChatsOffsetEnd := False;
  FLoading := True;
  TTask.Run(
    procedure
    begin
      VK.LoadUserInfo;
      TThread.Queue(nil,
        procedure
        begin
          TPreview.Instance.Subscribe(VK.UserPhoto100, FOnReadyAvatar);
          Caption := 'FMX VK Messanger [' + VK.UserName + ']';
        end);
      LoadConversationsAsync;
      Sleep(500);
      TThread.Queue(nil,
        procedure
        begin
          LoadDone;
        end);
    end);
end;

procedure TFormMain.VKLog(Sender: TObject; const Value: string);
begin
  Memo1.Lines.Add(Value);
end;

procedure TFormMain.VKLogin(Sender: TObject);
begin
  TFile.WriteAllText('token.tmp', VK.Token);
  TThread.Queue(nil, Reload);
end;

{ TChats }

function TChats.FindChat(const PeerId: TVkPeerId; var Frame: TFrameChat): Boolean;
begin
  for var Item in Self do
    if Item.ConversationId = PeerId then
    begin
      Frame := Item;
      Exit(True);
    end;
  Result := False;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

