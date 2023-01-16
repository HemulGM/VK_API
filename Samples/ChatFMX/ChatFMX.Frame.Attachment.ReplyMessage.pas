unit ChatFMX.Frame.Attachment.ReplyMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, VK.API, VK.Entity.Message,
  VK.Entity.Conversation, System.Messaging, ChatFMX.Frame.Attachment,
  VK.Entity.Common.ExtendedList;

type
  TFrameAttachmentReplyMessage = class(TFrameAttachment)
    LineLeft: TLine;
    RectangleImage: TRectangle;
    LayoutContent: TLayout;
    LayoutText: TLayout;
    LabelTitle: TLabel;
    LabelText: TLabel;
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FActive: Boolean;
    FTitle: string;
    FText: string;
    FImageUrl: string;
    FImageFile: string;
    FIsAttachmentText: Boolean;
    FOnSelect: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetImageUrl(const Value: string);
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetIsAttachmentText(const Value: Boolean);
    procedure SetOnSelect(const Value: TNotifyEvent);
  public
    property Active: Boolean read FActive write SetActive;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>);
    property Text: string read FText write SetText;
    property Title: string read FTitle write SetTitle;
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property IsAttachmentText: Boolean read FIsAttachmentText write SetIsAttachmentText;
    property OnSelect: TNotifyEvent read FOnSelect write SetOnSelect;
  end;

implementation

uses
  VK.Types, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common,
  ChatFMX.PreviewManager, ChatFMX.Utils;

{$R *.fmx}

constructor TFrameAttachmentReplyMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  ImageUrl := '';
end;

destructor TFrameAttachmentReplyMessage.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentReplyMessage.Fill(Item: TVkMessage; Data: TVkEntityExtendedList<TVkMessage>);
begin
  Text := ParseMention(PrepareForPreview(Item.Text));
  Title := '';
  ImageUrl := '';
  IsAttachmentText := False;
  if PeerIdIsUser(Item.FromId) then
  begin
    var User: TVkProfile;
    if Data.GetProfileById(Item.FromId, User) then
      Title := User.FullName;
  end
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.FromId, Group) then
      Title := Group.Name;
  end;
  if Text.IsEmpty and (Length(Item.FwdMessages) > 0) then
  begin
    Text := Length(Item.FwdMessages).ToString + WordOfCount(Length(Item.FwdMessages), [' сообщение', ' сообщения', ' сообщенй']);
    IsAttachmentText := True;
  end;
  if Text.IsEmpty and (Length(Item.Attachments) > 0) then
  begin
    var Attachment := Item.Attachments[0];
    Text := AttachmentToText(Attachment.&Type);
    IsAttachmentText := True;
  end;
  if Text.IsEmpty and Assigned(Item.Geo) then
  begin
    Text := 'Карта';
    IsAttachmentText := True;
  end;
  if Length(Item.Attachments) > 0 then
  begin
    ImageUrl := Item.Attachments[0].GetPreviewUrl(50);
  end;
end;

procedure TFrameAttachmentReplyMessage.FrameClick(Sender: TObject);
begin
  if Assigned(OnSelect) then
    OnSelect(Self);
end;

procedure TFrameAttachmentReplyMessage.FrameMouseEnter(Sender: TObject);
begin
  Active := True;
end;

procedure TFrameAttachmentReplyMessage.FrameMouseLeave(Sender: TObject);
begin
  Active := False;
end;

procedure TFrameAttachmentReplyMessage.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if FActive then
    LineLeft.Stroke.Color := $FF7A7A7A
  else
    LineLeft.Stroke.Color := $FF454545;
end;

procedure TFrameAttachmentReplyMessage.SetImageUrl(const Value: string);
begin
  FImageUrl := Value;
  if not FImageUrl.IsEmpty then
  begin
    RectangleImage.Visible := True;
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
    RectangleImage.Visible := False;
end;

procedure TFrameAttachmentReplyMessage.SetIsAttachmentText(const Value: Boolean);
begin
  FIsAttachmentText := Value;
  if FIsAttachmentText then
    LabelText.FontColor := $FF71AAEB
  else
    LabelText.FontColor := $FFE1E3E6;
end;

procedure TFrameAttachmentReplyMessage.SetOnSelect(const Value: TNotifyEvent);
begin
  FOnSelect := Value;
end;

procedure TFrameAttachmentReplyMessage.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    RectangleImage.Fill.Kind := TBrushKind.Solid;
  end
  else
  try
    RectangleImage.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleImage.Fill.Kind := TBrushKind.Bitmap;
    RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    RectangleImage.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentReplyMessage.SetText(const Value: string);
begin
  FText := Value;
  LabelText.Text := FText;
end;

procedure TFrameAttachmentReplyMessage.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

end.

