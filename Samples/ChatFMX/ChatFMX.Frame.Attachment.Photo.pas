unit ChatFMX.Frame.Attachment.Photo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, VK.Entity.Photo, FMX.Objects, VK.Types, System.Messaging;

type
  TFrameAttachmentPhoto = class(TFrame)
    Image: TImage;
    procedure FrameResize(Sender: TObject);
  private
    FVK: TCustomVK;
    FImageUrl: string;
    FImageFile: string;
    FWasImage: Boolean;
    FMaxWidth: Integer;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
    procedure SetMaxWidth(const Value: Integer);
  protected
    procedure Disappear; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Photo: TVkPhoto);
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading, VK.Photos, VK.Entity.Common;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentPhoto.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FWasImage := False;
  FVK := AVK;
  Name := '';
end;

destructor TFrameAttachmentPhoto.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentPhoto.Disappear;
begin
  inherited;
  Image.Bitmap := nil;
end;

procedure TFrameAttachmentPhoto.Fill(Photo: TVkPhoto);
var
  PhotoUrl: string;
begin
  if Assigned(Size) then
    MaxWidth := Photo.Sizes.GetMaxSizeOrZero.Width
  else
    MaxWidth := 1000;
  PhotoUrl := Photo.ToStringId;
  Height := 100;
  var Size := Photo.Sizes.GetSizeFromHeight(400);
  if Assigned(Size) then
  begin
    Width := Height * (Size.Width / Size.Height);
    if Size.Height < Height then
      Height := Width * (Size.Height / Size.Width);
    FImageUrl := Size.Url;
    if not FImageUrl.IsEmpty then
      TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end
  else
    Width := 80;
end;

procedure TFrameAttachmentPhoto.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
    Image.Bitmap := nil;
  end
  else
  try
    Image.Bitmap.LoadFromFile(FImageFile);
    FWasImage := True;
    //CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    //CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    Image.Bitmap := nil;
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TFrameAttachmentPhoto.FrameResize(Sender: TObject);
begin
  if Width > MaxWidth then
    Width := MaxWidth;
end;

procedure TFrameAttachmentPhoto.Paint;
begin
  inherited;
  if FWasImage and (Image.Bitmap.IsEmpty) then
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
end;

procedure TFrameAttachmentPhoto.SetMaxWidth(const Value: Integer);
begin
  FMaxWidth := Value;
end;

end.

