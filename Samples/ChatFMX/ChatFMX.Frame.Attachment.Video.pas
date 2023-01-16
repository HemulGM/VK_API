unit ChatFMX.Frame.Attachment.Video;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, VK.Types, System.Messaging, VK.Entity.Video, FMX.Layouts,
  FMX.Controls.Presentation, Skia, Skia.FMX, ChatFMX.Frame.Attachment, FMX.Ani;

type
  TFrameAttachmentVideo = class(TFrameAttachment)
    Image: TImage;
    LayoutInfo: TLayout;
    RectangleTime: TRectangle;
    LayoutInfoB: TLayout;
    LabelTime: TLabel;
    PathPlay: TPath;
    LayoutCaption: TLayout;
    LabelCaption: TLabel;
    LabelViews: TLabel;
    LayoutPlay: TLayout;
    CirclePlay: TCircle;
    ImagePlay: TImage;
    ColorAnimationOverPlay: TColorAnimation;
    procedure LabelTimeResize(Sender: TObject);
    procedure LayoutCaptionMouseEnter(Sender: TObject);
    procedure LayoutCaptionMouseLeave(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FShowCaption: Boolean;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetShowCaption(const Value: Boolean);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Video: TVkVideo);
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
  end;

implementation

uses
  ChatFMX.PreviewManager, ChatFMX.Utils, HGM.Common.DateUtils, System.Threading,
  VK.Entity.Common, VK.Video;

{$R *.fmx}

{ TFrameAttachmentPhoto }

procedure TFrameAttachmentVideo.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  LayoutCaption.Visible := FShowCaption;
end;

procedure TFrameAttachmentVideo.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    if (not FImageUrl.IsEmpty) and (Image.Bitmap.IsEmpty) then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
  begin
    Image.Bitmap := nil;
  end;
end;

constructor TFrameAttachmentVideo.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  ShowCaption := False;
end;

destructor TFrameAttachmentVideo.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentVideo.Fill(Video: TVkVideo);
var
  Id: string;
begin
  Id := Video.ToStringId;
  LabelCaption.Text := Video.Title;
  LabelViews.Text := Video.Views.ToString + ' ' + WordOfCount(Video.Views, ['просмотр', 'просмотра', 'просмотров']);
  PathPlay.Visible := Video.&Platform.IsEmpty;
  LabelTime.Text := SecondsToMinFormat(Video.Duration);
  if not Video.&Platform.IsEmpty then
    LabelTime.Text := Video.&Platform + ' · ' + LabelTime.Text;
  Height := 100;
  FImageUrl := '';
  for var Image in Video.Image do
    if Image.Height >= 300 then
    begin
      FImageUrl := Image.Url;
      Width := Height * (Image.Width / Image.Height);
      Break;
    end;
  if FImageUrl.IsEmpty and (Length(Video.Image) > 0) then
  begin
    var Image := Video.Image[High(Video.Image)];
    FImageUrl := Image.Url;
    Width := Height * (Image.Width / Image.Height);
  end;

  if FImageUrl.IsEmpty then
    Width := 80;
end;

procedure TFrameAttachmentVideo.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    Image.Bitmap := nil;
  end
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
  except
    Image.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentVideo.LabelTimeResize(Sender: TObject);
begin
  RectangleTime.Width := LabelTime.Width + 5 + PathPlay.Width + RectangleTime.Padding.Left + RectangleTime.Padding.Right;
end;

procedure TFrameAttachmentVideo.LayoutCaptionMouseEnter(Sender: TObject);
begin
  LabelCaption.Font.Style := LabelCaption.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentVideo.LayoutCaptionMouseLeave(Sender: TObject);
begin
  LabelCaption.Font.Style := LabelCaption.Font.Style - [TFontStyle.fsUnderline];
end;

end.

