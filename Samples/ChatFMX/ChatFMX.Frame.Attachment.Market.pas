unit ChatFMX.Frame.Attachment.Market;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  System.Messaging, ChatFMX.Frame.Attachment, FMX.Effects, FMX.Ani,
  VK.Entity.Market;

type
  TFrameAttachmentMarket = class(TFrameAttachment)
    LayoutText: TLayout;
    LabelTitle: TLabel;
    LabelDescription: TLabel;
    Rectangle1: TRectangle;
    RectangleImage: TRectangle;
    RectangleFrame: TRectangle;
    LayoutPrice: TLayout;
    LayoutButtons: TLayout;
    LabelPrice: TLabel;
    LabelPriceOld: TLabel;
    ButtonOpen: TButton;
    LayoutActions: TLayout;
    LayoutFavorite: TLayout;
    PathStar: TPath;
    ShadowEffectStar: TShadowEffect;
    FloatAnimationStarShadow: TFloatAnimation;
    RectangleBG: TRectangle;
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
    procedure SetVisibility(const Value: Boolean); override;
    procedure Fill(Item: TVkProduct);
    property IsFavorite: Boolean read FIsFavorite write SetIsFavorite;
  end;

implementation

uses
  ChatFMX.PreviewManager, Vk.Entity.Common;

{$R *.fmx}

{ TFrameAttachmentLink }

constructor TFrameAttachmentMarket.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  Rectangle1.Visible := False;
  RectangleFrame.Visible := True;
end;

destructor TFrameAttachmentMarket.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentMarket.SetVisibility(const Value: Boolean);
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

procedure TFrameAttachmentMarket.Fill(Item: TVkProduct);
begin
  FUrl := Item.Url;
  IsFavorite := Item.IsFavorite;
  LabelTitle.Text := Item.Title;
  LabelDescription.Text := Item.Description;
  if Assigned(Item.Price) then
  begin
    LabelPrice.Text := Item.Price.Text;
    LabelPriceOld.Text := Item.Price.OldAmountText;
    LayoutPrice.Visible := True;
  end
  else
  begin
    LayoutPrice.Visible := False;
  end;

  FImageUrl := Item.Thumb.GetSizeUrlOrEmpty(300);
end;

procedure TFrameAttachmentMarket.FOnReadyImage(const Sender: TObject; const M: TMessage);
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

procedure TFrameAttachmentMarket.SetIsFavorite(const Value: Boolean);
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

