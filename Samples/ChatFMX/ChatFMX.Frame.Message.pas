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
  ChatFMX.Frame.Attachment.ReplyMessage;

type
  {$IFDEF DEBUG_ADAPTIVE}
    {$DEFINE ANDROID}
  {$ENDIF}

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
    LabelGiftFrom: TLabel;
    RectangleGiftBG: TRectangle;
    LayoutFwdMessages: TLayout;
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
    procedure CreateReplyMessage(Item: TVkMessage; Data: TVkMessageHistory);
    procedure SetIsGift(const Value: Boolean);
    procedure CreateGift(Items: TVkAttachmentArray; Msg: TVkMessage);
    procedure SetCanAnswer(const Value: Boolean);
    procedure CreateFwdMessages(Items: TArray<TVkMessage>; Data: TVkMessageHistory);
    procedure CreateLinks(Items: TVkAttachmentArray);
    procedure CreatePosts(Items: TVkAttachmentArray; Data: TVkMessageHistory);
    procedure SetFromSex(const Value: TVkSex);
    procedure CreateCalls(Items: TVkAttachmentArray);
    procedure SetVisibility(const Value: Boolean);
    procedure BroadcastVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Item: TVkMessage; Data: TVkMessageHistory; ACanAnswer: Boolean);
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
  end;

implementation

uses
  System.Math, System.DateUtils, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager, ChatFMX.Frame.Attachment.Photo, ChatFMX.Utils,
  ChatFMX.Frame.Attachment.Sticker, ChatFMX.Frame.Attachment.Video,
  ChatFMX.Frame.Attachment.Gift, ChatFMX.Frame.Attachment.Message,
  ChatFMX.Frame.Attachment.Link, ChatFMX.Frame.Attachment.Wall,
  ChatFMX.Frame.Attachment.Call, ChatFMX.Frame.Attachment;

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
  LayoutFwdMessages.Visible := False;
  ClearMedia;
end;

destructor TFrameMessage.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameMessage.Fill(Item: TVkMessage; Data: TVkMessageHistory; ACanAnswer: Boolean);
begin
  ChatCanAnswer := ACanAnswer;
  Text := Item.Text;
  Date := Item.Date;
  ImageUrl := '';
  IsSelfMessage := Item.FromId = FVK.UserId;
  IsCanEdit := HoursBetween(Now, Item.Date) < 24;
  IsImportant := Item.Important;
  IsUnread := False;
  UpdateTime := Item.UpdateTime;
  IsGift := False;

  var P2P: Boolean := False;
  if Length(Data.Conversations) > 0 then
  begin
    P2P := Data.Conversations[0].IsUser;
    IsUnread := Item.Id > Data.Conversations[0].OutRead;
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
    CreateDocs(Item.Attachments);
    CreateAutioMessages(Item.Attachments, Item);
    CreateSticker(Item.Attachments);
    CreateGift(Item.Attachments, Item);
    CreateLinks(Item.Attachments);
    CreatePosts(Item.Attachments, Data);
    CreateCalls(Item.Attachments);
    RecalcMedia;
  end;

  if Assigned(Item.Geo) then
    CreateGeo(Item.Geo);

  if Length(Item.FwdMessages) > 0 then
    CreateFwdMessages(Item.FwdMessages, Data);

  RecalcSize;
end;

procedure TFrameMessage.CreatePosts(Items: TVkAttachmentArray; Data: TVkMessageHistory);
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

procedure TFrameMessage.CreateFwdMessages(Items: TArray<TVkMessage>; Data: TVkMessageHistory);
begin
  for var Item in Items do
  begin
    var Frame := TFrameAttachmentMessage.Create(LayoutFwdMessages, FVK);
    Frame.OnSelect := FOnAttachSelect;
    Frame.Parent := LayoutFwdMessages;
    Frame.Position.Y := 10000;
    Frame.Align := TAlignLayout.Top;
    Frame.Fill(Item, Data, ChatCanAnswer, True);
  end;
  LayoutFwdMessages.Visible := True;
  LayoutFwdMessages.RecalcSize;
end;

procedure TFrameMessage.CreateReplyMessage(Item: TVkMessage; Data: TVkMessageHistory);
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

procedure TFrameMessage.CreatePhotos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
  begin
    if Item.&Type = TVkAttachmentType.Photo then
    begin
      var Frame := TFrameAttachmentPhoto.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
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
  var Sz: Single := LayoutContent.Padding.Top + LayoutContent.Padding.Bottom;
  RecalcMedia;
  for var Control in LayoutClient.Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Sz := Max(Sz, 60);
  if Height <> Floor(Sz) then
    Height := Floor(Sz);
  if Assigned(ParentControl) then
    ParentControl.RecalcSize;
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
  MemoText.Height := MemoText.ContentSize.Size.Height + 5;
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

procedure TFrameMessage.SetCanAnswer(const Value: Boolean);
begin
  FCanAnswer := Value;
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
  LabelGiftFrom.Text := WordOfSex(FFromSex, ['отправил', 'отправила']) + ' подарок';
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

procedure TFrameMessage.SetIsGift(const Value: Boolean);
begin
  FIsGift := Value;
  MemoText.Visible := not FIsGift;
  LabelGiftFrom.Visible := FIsGift;
  if FIsGift then
  begin
    PathSelected.Fill.Color := $FFe3d3ac;
    LabelFrom.FontColor := TAlphaColorRec.White;
    LabelTime.FontColor := $FFE3D3AC;

    PathAnswer.Fill.Color := $99FFFFFF;
    ColorAnimationAnswer.StartValue := $99FFFFFF;
    ColorAnimationAnswer.StopValue := $EEFFFFFF;

    PathStar.Fill.Color := $99FFFFFF;
    ColorAnimationStar.StartValue := $99FFFFFF;
    ColorAnimationStar.StopValue := $EEFFFFFF;

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

    PathStar.Fill.Color := $FF5B5B5B;
    ColorAnimationStar.StartValue := $FF5B5B5B;
    ColorAnimationStar.StopValue := $FF6A6A6A;

    PathEdit.Fill.Color := $FF5B5B5B;
    ColorAnimationEdit.StartValue := $FF5B5B5B;
    ColorAnimationEdit.StopValue := $FF6A6A6A;
  end;
  RectangleGiftBG.Visible := FIsGift;
end;

procedure TFrameMessage.SetIsImportant(const Value: Boolean);
begin
  FIsImportant := Value;
  if FIsImportant then
  begin
    PathStar.Fill.Color := $FF71AAEB;
    ColorAnimationStar.StartValue := $FF71AAEB;
    ColorAnimationStar.StopValue := $FF71AAEB;
  end
  else
  begin
    PathStar.Fill.Color := $FF5B5B5B;
    ColorAnimationStar.StartValue := $FF5B5B5B;
    ColorAnimationStar.StopValue := $FF6A6A6A;
  end;
  LayoutFavorite.Visible := FIsImportant;
end;

procedure TFrameMessage.SetIsSelected(const Value: Boolean);
var
  Changed: Boolean;
begin
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

procedure TFrameMessage.SetMouseFrame(const Value: Boolean);
begin
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

