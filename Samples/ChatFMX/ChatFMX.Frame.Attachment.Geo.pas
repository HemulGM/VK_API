unit ChatFMX.Frame.Attachment.Geo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API, FMX.Objects, VK.Types, System.Messaging, VK.Entity.Geo,
  ChatFMX.Frame.Attachment;

type
  TFrameAttachmentGeo = class(TFrameAttachment)
    RectangleMap: TRectangle;
  private
    FImageUrl: string;
    FImageFile: string;
    procedure FOnReadyImage(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    destructor Destroy; override;
    procedure Fill(Geo: TVkGeo);
  end;

implementation

uses
  ChatFMX.PreviewManager, System.Threading;

{$R *.fmx}

{ TFrameAttachmentPhoto }

constructor TFrameAttachmentGeo.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
end;

destructor TFrameAttachmentGeo.Destroy;
begin
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  inherited;
end;

procedure TFrameAttachmentGeo.Fill(Geo: TVkGeo);
var
  Lat, Long: string;
begin
  if Assigned(Geo.Place) then
    RectangleMap.Hint := Geo.Place.Title;
  if Assigned(Geo.Coordinates) then
  begin
    Lat := Geo.Coordinates.Latitude.ToString.Replace(',', '.');
    Long := Geo.Coordinates.Longitude.ToString.Replace(',', '.');
    FImageUrl :=
      'https://static-maps.yandex.ru/1.x/?ll=' + Long + ',' + Lat + '&size=340,127&z=12&lang=ru_RU&l=map&pt=' + Long + ',' + Lat + ',vkbkm';
    TPreview.Instance.Subscribe(FImageUrl, FOnReadyImage);
  end;
end;

procedure TFrameAttachmentGeo.FOnReadyImage(const Sender: TObject; const M: TMessage);
var
  Data: TMessagePreview absolute M;
begin
  if Data.Value.Url <> FImageUrl then
    Exit;
  TPreview.Instance.Unsubscribe(FOnReadyImage);
  FImageFile := Data.Value.FileName;
  if FImageFile.IsEmpty then
  begin
    RectangleMap.Fill.Kind := TBrushKind.Solid;
  end
  else
  try
    RectangleMap.Fill.Bitmap.Bitmap.LoadFromFile(FImageFile);
    RectangleMap.Fill.Kind := TBrushKind.Bitmap;
    RectangleMap.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  except
    RectangleMap.Fill.Kind := TBrushKind.Solid;
  end;
end;

end.

