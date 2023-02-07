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
    CircleVideo: TCircle;
    procedure LabelTimeResize(Sender: TObject);
    procedure LayoutCaptionMouseEnter(Sender: TObject);
    procedure LayoutCaptionMouseLeave(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FShowCaption: Boolean;
    FIsCircle: Boolean;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetIsCircle(const Value: Boolean);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Video: TVkVideo);
    property IsCircle: Boolean read FIsCircle write SetIsCircle;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
  end;

implementation

uses
  ChatFMX.PreviewManager, ChatFMX.Utils, HGM.Common.DateUtils, System.Threading,
  VK.Entity.Common, VK.Video;

{$R *.fmx}

{ TFrameAttachmentPhoto }

procedure TFrameAttachmentVideo.SetIsCircle(const Value: Boolean);
begin
  FIsCircle := Value;
  Image.Visible := not FIsCircle;
  CircleVideo.Visible := FIsCircle;
end;

procedure TFrameAttachmentVideo.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  LayoutCaption.Visible := FShowCaption and not IsCircle;
end;

procedure TFrameAttachmentVideo.SetVisibility(const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    case IsCircle of
      False:
        if (not FImageUrl.IsEmpty) and (Image.Bitmap.IsEmpty) then
          TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
      True:
        if (not FImageUrl.IsEmpty) and (CircleVideo.Fill.Bitmap.Bitmap.IsEmpty) then
          TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
    end;
  end
  else
  begin
    Image.Bitmap := nil;
    CircleVideo.Fill.Bitmap.Bitmap := nil;
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
  IsCircle := Video.Title.StartsWith('Видеосообщение от @', True);
  LabelViews.Text := Video.Views.ToString + ' ' + WordOfCount(Video.Views, ['просмотр', 'просмотра', 'просмотров']);
  PathPlay.Visible := Video.&Platform.IsEmpty;
  LabelTime.Text := SecondsToMinFormat(Video.Duration);
  if not Video.&Platform.IsEmpty then
    LabelTime.Text := Video.&Platform + ' · ' + LabelTime.Text;

  Height := 100;
  FImageUrl := '';
  for var Image in Video.Image do
    if (Image.Height = 320) and (Image.Width = 320) then
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
  if IsCircle then
  begin
    Height := 216;
    Width := 216;
  end;
end;

procedure TFrameAttachmentVideo.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  case IsCircle of
    False:
      begin
        if FImageFile.IsEmpty then
          Image.Bitmap := nil
        else
        try
          Image.Bitmap.LoadFromFile(FImageFile);
        except
          Image.Bitmap := nil;
        end;
      end;
    True:
      begin
        if FImageFile.IsEmpty then
        begin
          CircleVideo.Fill.Kind := TBrushKind.Solid;
          CircleVideo.Fill.Color := TAlphaColorF.Create(1, 1, 1, 0.3).ToAlphaColor;
        end
        else
        try
          CircleVideo.Fill.Kind := TBrushKind.Bitmap;
          CircleVideo.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
          CircleVideo.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
        except
          CircleVideo.Fill.Kind := TBrushKind.Solid;
          CircleVideo.Fill.Color := TAlphaColorF.Create(1, 1, 1, 0.3).ToAlphaColor;
        end;
      end;
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

