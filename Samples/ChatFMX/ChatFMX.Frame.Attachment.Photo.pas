unit ChatFMX.Frame.Attachment.Photo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, VK.Entity.Photo, FMX.Objects, VK.Types, System.Messaging;

type
  TFrameAttachmentPhoto = class(TFrame)
    Image: TImage;
  private
    FVK: TCustomVK;
    FImageUrl: string;
    FImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    destructor Destroy; override;
    procedure Fill(Photo: TVkPhoto);
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading, VK.Photos, VK.Entity.Common;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentPhoto.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

destructor TFrameAttachmentPhoto.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentPhoto.Fill(Photo: TVkPhoto);
var
  PhotoUrl: string;
begin
  PhotoUrl := Photo.ToStringId;
  Height := 100;
  var Size := Photo.Sizes.GetSizeMin('o');
  if Assigned(Size) then
  begin
    Width := Height * (Size.Width / Size.Height);
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
    //CircleAvatar.Fill.Kind := TBrushKind.Bitmap;
    //CircleAvatar.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    Image.Bitmap := nil;
    //CircleAvatar.Fill.Kind := TBrushKind.Solid;
  end;
end;

end.

