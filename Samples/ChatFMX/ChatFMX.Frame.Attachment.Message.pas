unit ChatFMX.Frame.Attachment.Message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Memo.Types, FMX.Objects, FMX.Ani,
  FMX.ScrollBox, FMX.Memo, VK.API, VK.Entity.Message, VK.Entity.Conversation,
  ChatFMX.Frame.Attachment.Photo, ChatFMX.Frame.Attachment.Video,
  System.Messaging, VK.Entity.Media, VK.Entity.Geo, ChatFMX.Frame.Attachment;

type
  TFrameAttachmentMessage = class(TFrameAttachment)
    LayoutContent: TLayout;
    LayoutClient: TLayout;
    MemoText: TMemo;
    FlowLayoutMedia: TFlowLayout;
    LayoutUpdated: TLayout;
    LabelUpdated: TLabel;
    LayoutHead: TLayout;
    CircleAvatar: TCircle;
    LayoutFrom: TLayout;
    LabelFrom: TLabel;
    LayoutDetails: TLayout;
    LabelTime: TLabel;
    Rectangle1: TRectangle;
    LineLeft: TLine;
    procedure FrameResize(Sender: TObject);
    procedure MemoTextResize(Sender: TObject);
    procedure MemoTextChange(Sender: TObject);
    procedure MemoTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LabelFromMouseLeave(Sender: TObject);
    procedure LabelFromMouseEnter(Sender: TObject);
  private
    FFromText: string;
    FImageUrl: string;
    FImageFile: string;
    FText: string;
    FCanAnswer: Boolean;
    FDate: TDateTime;
    FUpdateTime: TDateTime;
    FWasSelectedText: Boolean;
    FOnSelect: TNotifyEvent;
    procedure SetFromText(const Value: string);
    procedure SetImageUrl(const Value: string);
    procedure SetText(const Value: string);
    procedure ClearMedia;
    procedure RecalcMedia;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetCanAnswer(const Value: Boolean);
    procedure SetDate(const Value: TDateTime);
    procedure SetUpdateTime(const Value: TDateTime);
    procedure CreateReplyMessage(Item: TVkMessage; Data: TVkMessageHistory);
    procedure CreateFwdMessages(Items: TArray<TVkMessage>; Item: TVkMessage; Data: TVkMessageHistory);
    procedure DoSelect;
    procedure SetOnSelect(const Value: TNotifyEvent);
    procedure CreatePhotos(Items: TVkAttachmentArray);
    procedure CreateVideos(Items: TVkAttachmentArray);
    procedure CreateGift(Items: TVkAttachmentArray; Msg: TVkMessage);
    procedure CreateAudios(Items: TVkAttachmentArray);
    procedure CreateAutioMessages(Items: TVkAttachmentArray; Msg: TVkMessage);
    procedure CreateDocs(Items: TVkAttachmentArray);
    procedure CreateLinks(Items: TVkAttachmentArray);
    procedure CreateSticker(Items: TVkAttachmentArray);
    procedure CreateGeo(Value: TVkGeo);
    procedure CreatePosts(Items: TVkAttachmentArray; Data: TVkMessageHistory; Fwd: Boolean);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkMessage; Data: TVkMessageHistory; ACanAnswer, Fwd: Boolean);
    property Text: string read FText write SetText;
    property Date: TDateTime read FDate write SetDate;
    property FromText: string read FFromText write SetFromText;
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property ChatCanAnswer: Boolean read FCanAnswer write SetCanAnswer;
    property UpdateTime: TDateTime read FUpdateTime write SetUpdateTime;
    property OnSelect: TNotifyEvent read FOnSelect write SetOnSelect;
  end;

implementation

uses
  System.Math, FMX.Memo.Style, ChatFMX.PreviewManager, VK.Types,
  VK.Entity.Profile, VK.Entity.Group, ChatFMX.Utils,
  ChatFMX.Frame.Attachment.Messages, ChatFMX.Frame.Attachment.Document,
  ChatFMX.Frame.Attachment.Gift, ChatFMX.Frame.Attachment.Sticker,
  ChatFMX.Frame.Attachment.AudioMessage, ChatFMX.Frame.Attachment.Audio,
  ChatFMX.Frame.Attachment.Link, ChatFMX.Frame.Attachment.Geo,
  ChatFMX.Frame.Attachment.Wall, ChatFMX.Frame.Attachment.WallFwd;

{$R *.fmx}

{ TFrameAttachmentMessage }

procedure TFrameAttachmentMessage.SetVisibility(const Value: Boolean);
begin
  inherited;
  for var Control in LayoutClient.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
  for var Control in FlowLayoutMedia.Controls do
    if Control is TFrameAttachment then
      (Control as TFrameAttachment).SetVisibility(Value);
end;

constructor TFrameAttachmentMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  {$IFDEF ANDROID}
  CircleAvatar.Margins.Right := 7;
  {$ENDIF}
  //RectangleBG.Visible := False;
  MemoText.DisableDisappear := True;
  ClearMedia;
end;

procedure TFrameAttachmentMessage.ClearMedia;
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

procedure TFrameAttachmentMessage.RecalcMedia;
begin
  var H: Single := 0;
  for var Control in FlowLayoutMedia.Controls do
  begin
    if (Control.Width > FlowLayoutMedia.Width) or
      ((FlowLayoutMedia.ControlsCount = 1) and (
      (FlowLayoutMedia.Controls[0] is TFrameAttachmentPhoto) or
      (FlowLayoutMedia.Controls[0] is TFrameAttachmentVideo)))
      then
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

destructor TFrameAttachmentMessage.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentMessage.FOnReadyImage(const Sender: TObject; const M: TMessage);
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

procedure TFrameAttachmentMessage.DoSelect;
begin
  if Assigned(OnSelect) then
    OnSelect(Self);
end;

procedure TFrameAttachmentMessage.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ANDROID}
  MemoText.SelLength := 0;
  DoSelect;
  {$ENDIF}
end;

procedure TFrameAttachmentMessage.FrameResize(Sender: TObject);
begin
  var Sz: Single := LayoutContent.Padding.Top + LayoutContent.Padding.Bottom;
  RecalcMedia;
  MemoTextChange(nil);
  for var Control in LayoutClient.Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Sz := Max(Sz, LayoutHead.Height);
  if Height <> Floor(Sz) then
  begin
    Height := Floor(Sz);
    if Assigned(ParentControl) then
      ParentControl.RecalcSize;
  end;
end;

procedure TFrameAttachmentMessage.LabelFromMouseEnter(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentMessage.LabelFromMouseLeave(Sender: TObject);
var
  Control: TLabel absolute Sender;
begin
  Control.TextSettings.Font.Style := Control.TextSettings.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentMessage.MemoTextChange(Sender: TObject);
begin
  var H := MemoText.ContentSize.Size.Height + 5;
  if H <> MemoText.Height then
  begin
    MemoText.Height := H;
    FrameResize(nil);
  end;
end;

procedure TFrameAttachmentMessage.MemoTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FWasSelectedText := MemoText.SelLength > 0;
end;

procedure TFrameAttachmentMessage.MemoTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (MemoText.SelLength > 0) or FWasSelectedText then
    Exit;
  {$IFNDEF ANDROID}
  FrameMouseUp(Sender, Button, Shift, X, Y);
  {$ENDIF}
end;

procedure TFrameAttachmentMessage.MemoTextResize(Sender: TObject);
begin
  MemoTextChange(Sender);
end;

procedure TFrameAttachmentMessage.Fill(Item: TVkMessage; Data: TVkMessageHistory; ACanAnswer, Fwd: Boolean);
begin
  ChatCanAnswer := ACanAnswer;
  Text := ParseMention(Item.Text);
  Date := Item.Date;
  ImageUrl := '';
  UpdateTime := Item.UpdateTime;

  var P2P: Boolean := False;
  if Length(Data.Conversations) > 0 then
  begin
    P2P := Data.Conversations[0].IsUser;
  end;
  if PeerIdIsUser(Item.FromId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.FromId, User) then
    begin
      if P2P then
        FromText := User.FirstName
      else
        FromText := User.FullName;
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
    CreateDocs(Item.Attachments);
    CreateAutioMessages(Item.Attachments, Item);
    CreateSticker(Item.Attachments);
    CreateGift(Item.Attachments, Item);
    CreateLinks(Item.Attachments);
    CreatePosts(Item.Attachments, Data, Fwd);
    RecalcMedia;
  end;

  if Assigned(Item.Geo) then
    CreateGeo(Item.Geo);

  if Length(Item.FwdMessages) > 0 then
    CreateFwdMessages(Item.FwdMessages, Item, Data);

  RecalcSize;
end;

procedure TFrameAttachmentMessage.CreatePosts(Items: TVkAttachmentArray; Data: TVkMessageHistory; Fwd: Boolean);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Wall then
    begin
      if not Fwd then
      begin
        var Frame := TFrameAttachmentWall.Create(LayoutClient, VK);
        Frame.Parent := LayoutClient;
        Frame.Position.Y := 10000;
        Frame.Align := TAlignLayout.Top;
        Frame.Fill(Item.Wall, Data);
      end
      else
      begin
        var Frame := TFrameAttachmentWallFwd.Create(LayoutClient, VK);
        Frame.Parent := LayoutClient;
        Frame.Position.Y := 10000;
        Frame.Align := TAlignLayout.Top;
        Frame.Fill(Item.Wall);
      end;
    end;
end;

procedure TFrameAttachmentMessage.CreateGeo(Value: TVkGeo);
begin
  var Frame := TFrameAttachmentGeo.Create(LayoutClient, VK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(Value);
end;

procedure TFrameAttachmentMessage.CreatePhotos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
  begin
    if Item.&Type = TVkAttachmentType.Photo then
    begin
      var Frame := TFrameAttachmentPhoto.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Photo);
    end;
    if (Item.&Type = TVkAttachmentType.Doc) and (Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Doc, True);
    end;
  end;
end;

procedure TFrameAttachmentMessage.CreateSticker(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Sticker then
    begin
      var Frame := TFrameAttachmentSticker.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Sticker);
    end;
end;

procedure TFrameAttachmentMessage.CreateAutioMessages(Items: TVkAttachmentArray; Msg: TVkMessage);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.AudioMessage then
    begin
      var Frame := TFrameAttachmentAudioMessage.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.AudioMessage, Msg.WasListened);
    end;
end;

procedure TFrameAttachmentMessage.CreateAudios(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Audio then
    begin
      var Frame := TFrameAttachmentAudio.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Audio);
    end;
end;

procedure TFrameAttachmentMessage.CreateLinks(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Link then
    begin
      var Frame := TFrameAttachmentLink.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Link);
    end;
end;

procedure TFrameAttachmentMessage.CreateDocs(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if (Item.&Type = TVkAttachmentType.Doc) and (not Assigned(Item.Doc.Preview)) then
    begin
      var Frame := TFrameAttachmentDocument.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Doc, False);
    end;
end;

procedure TFrameAttachmentMessage.CreateVideos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Video then
    begin
      var Frame := TFrameAttachmentVideo.Create(FlowLayoutMedia, VK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Video);
    end;
end;

procedure TFrameAttachmentMessage.CreateGift(Items: TVkAttachmentArray; Msg: TVkMessage);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Gift then
    begin
      var Frame := TFrameAttachmentGift.Create(LayoutClient, VK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.Gift, Msg, ChatCanAnswer);
      Frame.IsMini := True;
    end;
end;

procedure TFrameAttachmentMessage.CreateReplyMessage(Item: TVkMessage; Data: TVkMessageHistory);
begin
  var Frame := TFrameAttachmentMessages.Create(LayoutClient, VK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(1, Item.Id, False);
end;

procedure TFrameAttachmentMessage.CreateFwdMessages(Items: TArray<TVkMessage>; Item: TVkMessage; Data: TVkMessageHistory);
begin
  var Frame := TFrameAttachmentMessages.Create(LayoutClient, VK);
  Frame.Parent := LayoutClient;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.Fill(Length(Items), Item.Id, True);
end;

procedure TFrameAttachmentMessage.SetCanAnswer(const Value: Boolean);
begin
  FCanAnswer := Value;
end;

procedure TFrameAttachmentMessage.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  LabelTime.Text := FormatDateTime('HH:nn', FDate);
  LabelTime.Hint := FormatDateTime('c', FDate);
end;

procedure TFrameAttachmentMessage.SetFromText(const Value: string);
begin
  FFromText := Value;
  LabelFrom.Text := FFromText;
end;

procedure TFrameAttachmentMessage.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if not FImageUrl.IsEmpty then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
  else
    CircleAvatar.Fill.Kind := TBrushKind.Solid;
end;

procedure TFrameAttachmentMessage.SetOnSelect(const Value: TNotifyEvent);
begin
  FOnSelect := Value;
end;

procedure TFrameAttachmentMessage.SetText(const Value: string);
begin
  FText := ParseMention(Value);
  MemoText.Visible := not FText.IsEmpty;
  if not FText.IsEmpty then
  begin
    MemoText.Text := FText;
    (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
    (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  end;
end;

procedure TFrameAttachmentMessage.SetUpdateTime(const Value: TDateTime);
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

end.

