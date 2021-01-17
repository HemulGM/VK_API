unit VK.Entity.Sticker;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common;

type
  TVkSticker = class
  private
    FImages: TArray<TVkImage>;
    FImages_with_background: TArray<TVkImage>;
    FProduct_id: Integer;
    FSticker_id: Integer;
  public
    property Images: TArray<TVkImage> read FImages write FImages;
    property ImagesWithBackground: TArray<TVkImage> read FImages_with_background write FImages_with_background;
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
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImages);
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImages_with_background);
  inherited;
end;

end.

