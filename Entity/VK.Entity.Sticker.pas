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
    property Height: Extended read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Extended read FWidth write FWidth;
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
    property Images: TArray<TVkStickerImage> read FImages write FImages;
    property ImagesWithBackground: TArray<TVkStickerImage> read FImages_with_background write FImages_with_background;
    property ProductId: Extended read FProduct_id write FProduct_id;
    property StickerId: Extended read FSticker_id write FSticker_id;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSticker;
  end;

implementation

uses
  VK.CommonUtils;

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
begin
  TArrayHelp.FreeArrayOfObject<TVkStickerImage>(FImages);
  TArrayHelp.FreeArrayOfObject<TVkStickerImage>(FImages_with_background);
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

