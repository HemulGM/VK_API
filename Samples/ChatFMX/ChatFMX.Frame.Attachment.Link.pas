unit ChatFMX.Frame.Attachment.Link;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  VK.Entity.Link, System.Messaging, ChatFMX.Frame.Attachment, FMX.Effects,
  FMX.Ani;

type
  TFrameAttachmentLink = class(TFrameAttachment)
    RectangleFrame: TRectangle;
    RectangleText: TRectangle;
    LayoutActions: TLayout;
    LabelTitle: TLabel;
    LabelUrl: TLabel;
    LayoutPreview: TLayout;
    LayoutShort: TLayout;
    LayoutText: TLayout;
    LabelShortTitle: TLabel;
    LabelShortUrl: TLabel;
    LineLeft: TLine;
    RectangleImage: TRectangle;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    PathStar: TPath;
    ShadowEffectStar: TShadowEffect;
    FloatAnimationStarShadow: TFloatAnimation;
    LayoutFavorite: TLayout;
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FUrl: string;
    FIsFavorite: Boolean;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetIsFavorite(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Item: TVkLink);
    property IsFavorite: Boolean read FIsFavorite write SetIsFavorite;
  end;

implementation

uses
  ChatFMX.PreviewManager, Vk.Entity.Common;

{$R *.fmx}

{ TFrameAttachmentLink }

constructor TFrameAttachmentLink.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  Rectangle1.Visible := False;
end;

destructor TFrameAttachmentLink.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentLink.Fill(Item: TVkLink);
begin
  FUrl := Item.Url;
  IsFavorite := Item.IsFavorite;
  if not Assigned(Item.Photo) then
  begin
    Height := 40;
    LayoutPreview.Visible := False;
    LayoutShort.Visible := True;
    LabelShortTitle.Text := Item.Title;
    if not Item.Caption.IsEmpty then
      LabelShortUrl.Text := Item.Caption
    else
      LabelShortUrl.Text := Item.Url;
  end
  else
  begin
    Height := 270;
    LayoutPreview.Visible := True;
    LayoutShort.Visible := False;
    LabelTitle.Text := Item.Title;
    if not Item.Caption.IsEmpty then
      LabelUrl.Text := Item.Caption
    else
      LabelUrl.Text := Item.Url;

    FImageUrl := Item.Photo.Sizes.GetSizeUrlOrEmpty(300);
    if not FImageUrl.IsEmpty then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end;
end;

procedure TFrameAttachmentLink.FOnReadyImage(const Sender: TObject; const M: TMessage);
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
    RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.Tile;
  except
    RectangleImage.Fill.Kind := TBrushKind.None;
  end
  else
    RectangleImage.Fill.Kind := TBrushKind.None;
end;

procedure TFrameAttachmentLink.FrameMouseEnter(Sender: TObject);
begin
  LabelTitle.Font.Style := LabelTitle.Font.Style + [TFontStyle.fsUnderline];
  LabelShortTitle.Font.Style := LabelShortTitle.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentLink.FrameMouseLeave(Sender: TObject);
begin
  LabelTitle.Font.Style := LabelTitle.Font.Style - [TFontStyle.fsUnderline];
  LabelShortTitle.Font.Style := LabelShortTitle.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TFrameAttachmentLink.SetIsFavorite(const Value: Boolean);
begin
  FIsFavorite := Value;
  if FIsFavorite then
  begin
    PathStar.Fill.Kind := TBrushKind.Solid;
    PathStar.Fill.Color := TAlphaColorRec.White;
    PathStar.Stroke.Kind := TBrushKind.None;
  end
  else
  begin
    PathStar.Fill.Kind := TBrushKind.None;
    PathStar.Stroke.Kind := TBrushKind.Solid;
    PathStar.Stroke.Color := TAlphaColorRec.White;
  end;
end;

end.

