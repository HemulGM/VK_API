unit ChatFMX.Frame.Attachment.Sticker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, VK.API, VK.Entity.Sticker, System.Messaging, Skia, Skia.FMX,
  FMX.Effects, FMX.Filter.Effects, ChatFMX.Frame.Attachment;

type
  TFrameAttachmentSticker = class(TFrameAttachment)
    ImageSticker: TImage;
    AnimatedImageSticker: TSkAnimatedImage;
    ShadowEffect1: TShadowEffect;
    procedure AnimatedImageStickerMouseEnter(Sender: TObject);
  private
    FImageUrl, FImageFile: string;
    FAnimImageUrl, FAnimImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnReadyImageAnim(const Sender: TObject; const M: TMessage);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Sticker: TVkSticker);
  end;

implementation

uses
  ChatFMX.PreviewManager;

{$R *.fmx}

{ TFrameAttachmentSticker }

procedure TFrameAttachmentSticker.SetVisibility(const Value: Boolean);
begin
  inherited;
  ImageSticker.Visible := False;
  AnimatedImageSticker.Visible := False;
  if Value then
  begin
    if FAnimImageUrl.IsEmpty then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage)
    else
      TPreview.Instance.Subscribe(FAnimImageUrl, FOnReadyImageAnim);
  end
  else
  begin
    ImageSticker.Bitmap := nil;
    AnimatedImageSticker.Source.Data := [];
  end;
end;

procedure TFrameAttachmentSticker.AnimatedImageStickerMouseEnter(Sender: TObject);
begin
  if not AnimatedImageSticker.RunningAnimation then
    AnimatedImageSticker.Progress := 0;
end;

constructor TFrameAttachmentSticker.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  ImageSticker.Visible := False;
  AnimatedImageSticker.Visible := False;
end;

destructor TFrameAttachmentSticker.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  TPreview.Instance.Unsubscribe(FOnReadyImageAnim);
  inherited;
end;

procedure TFrameAttachmentSticker.Fill(Sticker: TVkSticker);
begin
  if Sticker.AnimationUrl.IsEmpty then
    FImageUrl := Sticker.Images[High(Sticker.Images)].Url
  else
    FAnimImageUrl := Sticker.AnimationUrl;
end;

procedure TFrameAttachmentSticker.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    ImageSticker.Bitmap := nil;
    ImageSticker.Visible := False;
  end
  else
  try
    ImageSticker.Bitmap.LoadFromFile(FImageFile);
    ImageSticker.Visible := True;
  except
    ImageSticker.Bitmap := nil;
    ImageSticker.Visible := False;
  end;
end;

procedure TFrameAttachmentSticker.FOnReadyImageAnim(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FAnimImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImageAnim);
  FAnimImageFile := Data.Value.FileName;
  if FAnimImageFile.IsEmpty then
  begin
    AnimatedImageSticker.Visible := False;
  end
  else
  try
    AnimatedImageSticker.LoadFromFile(FAnimImageFile);
    AnimatedImageSticker.Visible := True;
  except
    AnimatedImageSticker.Visible := False;
  end;
end;

end.

