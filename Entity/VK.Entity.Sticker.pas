unit VK.Entity.Sticker;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo;

type
  TVkStickerImage = class
  private
    FHeight: Integer;
    FUrl: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Integer read FWidth write FWidth;
  end;

  TVkSticker = class
  private
    FImages: TArray<TVkStickerImage>;
    FImages_with_background: TArray<TVkStickerImage>;
    FProduct_id: Integer;
    FSticker_id: Integer;
  public
    property Images: TArray<TVkStickerImage> read FImages write FImages;
    property ImagesWithBackground: TArray<TVkStickerImage> read FImages_with_background write FImages_with_background;
    property ProductId: Integer read FProduct_id write FProduct_id;
    property StickerId: Integer read FSticker_id write FSticker_id;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkSticker}

destructor TVkSticker.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStickerImage>(FImages);
  TArrayHelp.FreeArrayOfObject<TVkStickerImage>(FImages_with_background);
  inherited;
end;

end.

