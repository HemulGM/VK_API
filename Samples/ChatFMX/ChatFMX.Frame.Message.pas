unit ChatFMX.Frame.Message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, VK.Entity.Message, VK.Entity.PushSettings, VK.API,
  VK.Entity.Conversation, FMX.Memo.Style, System.Messaging, FMX.Ani,
  VK.Entity.Media, VK.Types, ChatFMX.Frame.Attachment.AudioMessage;

type
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
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    procedure MemoTextChange(Sender: TObject);
    procedure MemoTextResize(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure LabelFromMouseEnter(Sender: TObject);
    procedure LabelFromMouseLeave(Sender: TObject);
    procedure CircleAvatarClick(Sender: TObject);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoTextExit(Sender: TObject);
    procedure MemoTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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
    procedure CreateAutioMessages(Items: TVkAttachmentArray);
    procedure CreateSticker(Items: TVkAttachmentArray);
    procedure CreateVideos(Items: TVkAttachmentArray);
    procedure SetDate(const Value: TDateTime);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Item: TVkMessage; Data: TVkMessageHistory);
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
  end;

implementation

uses
  System.Math, System.DateUtils, VK.Entity.Profile, VK.Entity.Group,
  ChatFMX.PreviewManager, ChatFMX.Frame.Attachment.Photo, ChatFMX.Utils,
  ChatFMX.Frame.Attachment.Sticker, ChatFMX.Frame.Attachment.Video;

{$R *.fmx}

procedure TFrameMessage.CircleAvatarClick(Sender: TObject);
begin
  //
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
  FlowLayoutMedia.RecalcSize;
  var H: Single := 0;
  for var Control in FlowLayoutMedia.Controls do
    H := Max(Control.Position.Y + Control.Height, H);
  FlowLayoutMedia.Height := H;
end;

constructor TFrameMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  Name := '';
  FVK := AVK;
  RectangleBG.Visible := False;
  RectangleUnread.Visible := False;
  PathSelected.Visible := False;
  MemoText.DisableDisappear := True;
  MouseFrame := False;
  IsSelected := False;
  ClearMedia;
end;

destructor TFrameMessage.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameMessage.Fill(Item: TVkMessage; Data: TVkMessageHistory);
begin
  Text := Item.Text;
  Date := Item.Date;
  ImageUrl := '';
  IsSelfMessage := Item.FromId = FVK.UserId;
  IsCanEdit := HoursBetween(Now, Item.Date) < 24;
  IsImportant := Item.Important;
  IsUnread := False;

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

  if Length(Item.Attachments) > 0 then
  begin
    CreatePhotos(Item.Attachments);
    CreateVideos(Item.Attachments);
    CreateAutioMessages(Item.Attachments);
    CreateSticker(Item.Attachments);
  end;
end;

procedure TFrameMessage.CreatePhotos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Photo then
    begin
      var Frame := TFrameAttachmentPhoto.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Photo);
      //Frame.Height := 80;
      //Frame.Width := 100;
    end;
  RecalcMedia;
end;

procedure TFrameMessage.CreateVideos(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.Video then
    begin
      var Frame := TFrameAttachmentVideo.Create(FlowLayoutMedia, FVK);
      Frame.Parent := FlowLayoutMedia;
      Frame.Fill(Item.Video);
      //Frame.Height := 80;
      //Frame.Width := 100;
    end;
  RecalcMedia;
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
      //Frame.Height := 147;
    end;
end;

procedure TFrameMessage.CreateAutioMessages(Items: TVkAttachmentArray);
begin
  for var Item in Items do
    if Item.&Type = TVkAttachmentType.AudioMessage then
    begin
      var Frame := TFrameAttachmentAudioMessage.Create(LayoutClient, FVK);
      Frame.Parent := LayoutClient;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.Fill(Item.AudioMessage);
      //Frame.Height := 36;
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
  MemoText.SelLength := 0;
  IsSelected := not IsSelected;
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  var Sz: Single := LayoutContent.Padding.Top + LayoutContent.Padding.Bottom;
  for var Control in LayoutClient.Controls do
    if Control.IsVisible then
      Sz := Sz + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  Sz := Max(Sz, 60);
  if Height <> Sz then
    Height := Sz;
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
  IsSelected := not IsSelected;
end;

procedure TFrameMessage.MemoTextResize(Sender: TObject);
begin
  MemoTextChange(Sender);
end;

procedure TFrameMessage.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  LabelTime.Text := FormatDateTime('HH:nn', FDate);
  LabelTime.Hint := FormatDateTime('c', FDate);
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
  LayoutAnswer.Visible := (not IsSelfMessage) and FMouseFrame;
  LayoutEdit.Visible := (IsSelfMessage and IsCanEdit) and FMouseFrame;
  if not IsImportant then
    LayoutFavorite.Visible := FMouseFrame;
end;

procedure TFrameMessage.SetOnSelectedChanged(const Value: TNotifyEvent);
begin
  FOnSelectedChanged := Value;
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
  if not FText.IsEmpty then
  begin
    MemoText.Text := FText;
    (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
    (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  end;
end;

end.

