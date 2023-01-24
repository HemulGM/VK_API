unit ChatFMX.Frame.Attachment.Story;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  System.Messaging, ChatFMX.Frame.Attachment, FMX.Effects, FMX.Ani,
  VK.Entity.Stories, VK.Entity.Common.ExtendedList, VK.Entity.Message;

type
  TFrameAttachmentStory = class(TFrameAttachment)
    LayoutText: TLayout;
    LabelTitle: TLabel;
    LabelDate: TLabel;
    Rectangle1: TRectangle;
    RectangleImage: TRectangle;
    RectangleFrame: TRectangle;
    LayoutButtons: TLayout;
    ButtonOpen: TButton;
    RectangleBG: TRectangle;
  private
    FImageUrl: string;
    FImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkStory; Data: TVkEntityExtendedList<TVkMessage>);
  end;

implementation

uses
  ChatFMX.PreviewManager, ChatFMX.Utils, Vk.Entity.Common, VK.Entity.Profile,
  VK.Entity.Group;

{$R *.fmx}

{ TFrameAttachmentLink }

constructor TFrameAttachmentStory.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  Rectangle1.Visible := False;
  RectangleFrame.Visible := True;
end;

destructor TFrameAttachmentStory.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentStory.Fill(Item: TVkStory; Data: TVkEntityExtendedList<TVkMessage>);
begin
  FImageUrl := '';
  var FromText: string;
  var User: TVkProfile;
  if Data.GetProfileById(Item.OwnerId, User) then
    FromText := ' от ' + User.FullNameGen
  else
  begin
    var Group: TVkGroup;
    if Data.GetGroupById(Item.OwnerId, Group) then
      FromText := ' от ' + Group.Name;
  end;
  LabelTitle.Text := 'История' + FromText;
  LabelDate.Text := HumanDateTimeSimple(Item.Date, True, True);

  if Assigned(Item.Photo) then
  begin
    FImageUrl := Item.Photo.Sizes.GetSizeUrlOrEmpty(300);
  end
  else if Assigned(Item.Video) then
  begin
    for var Image in Item.Video.Image do
      if (Image.Height >= 100) and (Image.Width >= 100) then
      begin
        FImageUrl := Image.Url;
        Break;
      end;
  end;
end;

procedure TFrameAttachmentStory.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    if (not FImageUrl.IsEmpty) and (RectangleImage.Fill.Bitmap.Bitmap.IsEmpty) then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
  begin
    RectangleImage.Fill.Bitmap.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentStory.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if not FImageFile.IsEmpty then
  try
    RectangleImage.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleImage.Fill.Kind := TBrushKind.Bitmap;
    RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    RectangleImage.Fill.Kind := TBrushKind.None;
  end
  else
    RectangleImage.Fill.Kind := TBrushKind.None;
end;

end.

