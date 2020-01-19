unit VK.Entity.Sticker;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkStickerImage = class
  private
    FHeight: Extended;
    FUrl: string;
    FWidth: Extended;
  public
    property height: Extended read FHeight write FHeight;
    property url: string read FUrl write FUrl;
    property width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStickerImage;
  end;

  TVkSticker = class
  private
    FImages: TArray<TVkStickerImage>;
    FImages_with_background: TArray<TVkStickerImage>;
    FProduct_id: Extended;
    FSticker_id: Extended;
  public
    property images: TArray<TVkStickerImage> read FImages write FImages;
    property images_with_background: TArray<TVkStickerImage> read FImages_with_background write FImages_with_background;
    property product_id: Extended read FProduct_id write FProduct_id;
    property sticker_id: Extended read FSticker_id write FSticker_id;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSticker;
  end;

implementation

{TVkStickerImage}

function TVkStickerImage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStickerImage.FromJsonString(AJsonString: string): TVkStickerImage;
begin
  result := TJson.JsonToObject<TVkStickerImage>(AJsonString)
end;

{TVkSticker}

destructor TVkSticker.Destroy;
var
  LimagesItem: TVkStickerImage;
begin

  for LimagesItem in FImages do
    LimagesItem.Free;
  for LimagesItem in FImages_with_background do
    LimagesItem.Free;

  inherited;
end;

function TVkSticker.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSticker.FromJsonString(AJsonString: string): TVkSticker;
begin
  result := TJson.JsonToObject<TVkSticker>(AJsonString)
end;

end.

