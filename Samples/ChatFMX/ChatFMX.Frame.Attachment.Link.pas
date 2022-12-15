unit ChatFMX.Frame.Attachment.Link;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ChatFMX.DM.Res, FMX.Layouts, VK.API,
  VK.Entity.Link, System.Messaging;

type
  TFrameAttachmentLink = class(TFrame)
    RectangleFrame: TRectangle;
    RectangleText: TRectangle;
    ButtonFavorite: TButton;
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
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
  private
    FVK: TCustomVK;
    FImageUrl: string;
    FImageFile: string;
    FUrl: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Item: TVkLink);
  end;

implementation

uses
  ChatFMX.PreviewManager, Vk.Entity.Common;

{$R *.fmx}

{ TFrameAttachmentLink }

constructor TFrameAttachmentLink.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
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
  if not Assigned(Item.Photo) then
  begin
    Height := 40;
    LayoutPreview.Visible := False;
    LayoutShort.Visible := True;
    LabelShortTitle.Text := Item.Title;
    LabelShortUrl.Text := Item.Caption;
  end
  else
  begin
    Height := 270;
    LayoutPreview.Visible := True;
    LayoutShort.Visible := False;
    LabelTitle.Text := Item.Title;
    LabelUrl.Text := Item.Caption;
    FImageUrl := Item.Photo.Sizes.GetSizeUrlOrEmpty(130);
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

end.

