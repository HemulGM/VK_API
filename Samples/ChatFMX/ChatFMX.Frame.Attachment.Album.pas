unit ChatFMX.Frame.Attachment.Album;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, VK.Entity.Photo, FMX.Objects, VK.Types, System.Messaging,
  ChatFMX.Frame.Attachment, Vk.Entity.Album, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Ani;

type
  TFrameAttachmentAlbum = class(TFrameAttachment)
    Image: TImage;
    LayoutDescription: TLayout;
    LayoutTitle: TLayout;
    LabelTitle: TLabel;
    LabelCount: TLabel;
    Rectangle1: TRectangle;
    LayoutDescAni: TLayout;
    LabelDesc: TLabel;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    procedure FrameResize(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure ImageMouseEnter(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
  private
    FImageUrl: string;
    FImageFile: string;
    FWasImage: Boolean;
    FMaxWidth: Integer;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetMaxWidth(const Value: Integer);
  public
    procedure SetVisibility(const Value: Boolean); override;
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Album: TVkPhotoAlbum);
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading, VK.Photos, VK.Entity.Common,
  System.Math, ChatFMX.Utils;

{$R *.fmx}

{ TFrameAttachmentAlbum }

constructor TFrameAttachmentAlbum.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
  FWasImage := False;
end;

destructor TFrameAttachmentAlbum.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentAlbum.Fill(Album: TVkPhotoAlbum);
begin
  Height := 250;
  LabelTitle.Text := Album.Title;
  LabelCount.Text := Album.Size.ToString;
  LabelDesc.Text := Album.Description;
  var Sizes: TVkSizes;
  if Assigned(Album.Thumb) then
    Sizes := Album.Thumb.Sizes
  else
    Sizes := Album.Sizes;

  MaxWidth := Sizes.GetMaxSizeOrZero.Width;
  if MaxWidth <= 0 then
    MaxWidth := 1000;

  var Size := Sizes.GetSizeFromHeight(400);
  if Assigned(Size) then
  begin
    Width := Height * (Size.Width / Size.Height);
    if Size.Height < Height then
      Height := Width * (Size.Height / Size.Width);
    FImageUrl := Size.Url;
  end
  else
    Width := 80;
end;

procedure TFrameAttachmentAlbum.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
    Image.Bitmap := nil
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
    FWasImage := True;
  except
    Image.Bitmap := nil;
  end;
end;

procedure TFrameAttachmentAlbum.FrameResize(Sender: TObject);
begin
  if Width > MaxWidth then
    Width := MaxWidth;
end;

procedure TFrameAttachmentAlbum.ImageClick(Sender: TObject);
begin
  //
end;

procedure TFrameAttachmentAlbum.ImageMouseEnter(Sender: TObject);
begin
  if LabelDesc.Text.IsEmpty then
    Exit;
  LayoutDescAni.Height := Min(LabelDesc.Height, Height - 70) + 9;
  LayoutDescAni.Margins.Bottom := -LayoutDescAni.Height;
  TAnimator.DetachPropertyAnimation(LayoutDescAni, 'Margins.Bottom');
  TAnimator.AnimateFloat(LayoutDescAni, 'Margins.Bottom', 0);
end;

procedure TFrameAttachmentAlbum.ImageMouseLeave(Sender: TObject);
begin
  if LabelDesc.Text.IsEmpty then
    Exit;
  TAnimator.DetachPropertyAnimation(LayoutDescAni, 'Margins.Bottom');
  TAnimator.AnimateFloat(LayoutDescAni, 'Margins.Bottom', -LayoutDescAni.Height);
end;

procedure TFrameAttachmentAlbum.SetMaxWidth(const Value: Integer);
begin
  FMaxWidth := Value;
end;

procedure TFrameAttachmentAlbum.SetVisibility(const Value: Boolean);
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

end.

