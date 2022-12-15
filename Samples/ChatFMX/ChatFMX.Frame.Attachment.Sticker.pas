﻿unit ChatFMX.Frame.Attachment.Sticker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, VK.API, VK.Entity.Sticker, System.Messaging, Skia, Skia.FMX,
  FMX.Effects, FMX.Filter.Effects;

type
  TFrameAttachmentSticker = class(TFrame)
    ImageSticker: TImage;
    AnimatedImageSticker: TSkAnimatedImage;
    ShadowEffect1: TShadowEffect;
    procedure AnimatedImageStickerMouseEnter(Sender: TObject);
  private
    FVK: TCustomVK;
    FImageUrl, FImageFile: string;
    FAnimImageUrl, FAnimImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure FOnReadyImageAnim(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Sticker: TVkSticker);
  end;

implementation

uses
  ChatFMX.PreviewManager;

{$R *.fmx}

{ TFrameAttachmentSticker }

procedure TFrameAttachmentSticker.AnimatedImageStickerMouseEnter(Sender: TObject);
begin
  if not AnimatedImageSticker.RunningAnimation then
    AnimatedImageSticker.Progress := 0;
end;

constructor TFrameAttachmentSticker.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
  ImageSticker.Visible := False;
  AnimatedImageSticker.Visible := False;
end;

destructor TFrameAttachmentSticker.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentSticker.Fill(Sticker: TVkSticker);
begin
  if Sticker.AnimationUrl.IsEmpty then
  begin
    FImageUrl := Sticker.Images[High(Sticker.Images)].Url;
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
  begin
    FAnimImageUrl := Sticker.AnimationUrl;
    TPreview.Instance.Subscribe(FAnimImageUrl, FOnReadyImageAnim);
  end;
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
