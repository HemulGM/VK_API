unit VK.Entity.Market;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common;

type
  TVkMarketSection = TVkBasicObject;

  TVkProductPrice = class
  private
    FAmount: string;
    FCurrency: TVkProductCurrency;
    FText: string;
  public
    property Amount: string read FAmount write FAmount;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProductPrice;
  end;

  TVkProductCategory = class
  private
    FId: Extended;
    FName: string;
    FSection: TVkMarketSection;
  public
    property Id: Extended read FId write FId;
    property Name: string read FName write FName;
    property Section: TVkMarketSection read FSection write FSection;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProductCategory;
  end;

  TVkProduct = class
  private
    FAvailability: Integer;
    FCategory: TVkProductCategory;
    FDate: Int64;
    FDescription: string;
    FExternal_id: string;
    FId: Integer;
    FOwner_id: Integer;
    FPrice: TVkProductPrice;
    FThumb_photo: string;
    FTitle: string;
    FPhotos: TArray<TVkPhoto>;
    FAlbums_ids: TArray<Integer>;
    FCan_comment: Integer;
    FCan_repost: Integer;
    FViews_count: Integer;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
  public
    property Availability: Integer read FAvailability write FAvailability;
    property Category: TVkProductCategory read FCategory write FCategory;
    property Date: Int64 read FDate write FDate;
    property Description: string read FDescription write FDescription;
    property ExternalId: string read FExternal_id write FExternal_id;
    property Id: Integer read FId write FId;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Price: TVkProductPrice read FPrice write FPrice;
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    property Title: string read FTitle write FTitle;
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    property AlbumsIds: TArray<Integer> read FAlbums_ids write FAlbums_ids;
    property CanComment: Integer read FCan_comment write FCan_comment;
    property CanRepost: Integer read FCan_repost write FCan_repost;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property ViewsCount: Integer read FViews_count write FViews_count;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProduct;
  end;

  TVkProducts = class
  private
    FItems: TArray<TVkProduct>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkProduct> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Audios: TVkProducts);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProducts;
  end;

  TVkMarketAlbum = class
  private
    FAvailability: Extended;
    FCategory: TVkProductCategory;
    FDate: Extended;
    FDescription: string;
    FExternal_id: string;
    FId: Extended;
    FOwner_id: Extended;
    FPrice: TVkProductPrice;
    FThumb_photo: string;
    FTitle: string;
  public
    property Availability: Extended read FAvailability write FAvailability;
    property Category: TVkProductCategory read FCategory write FCategory;
    property Date: Extended read FDate write FDate;
    property Description: string read FDescription write FDescription;
    property ExternalId: string read FExternal_id write FExternal_id;
    property Id: Extended read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property Price: TVkProductPrice read FPrice write FPrice;
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    property Title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMarketAlbum;
  end;

implementation

{TVkMarketPrice}

constructor TVkProductPrice.Create;
begin
  inherited;
  FCurrency := TVkProductCurrency.Create();
end;

destructor TVkProductPrice.Destroy;
begin
  FCurrency.Free;
  inherited;
end;

function TVkProductPrice.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkProductPrice.FromJsonString(AJsonString: string): TVkProductPrice;
begin
  result := TJson.JsonToObject<TVkProductPrice>(AJsonString)
end;

{TVkMarketCategory}

constructor TVkProductCategory.Create;
begin
  inherited;
  FSection := TVkMarketSection.Create();
end;

destructor TVkProductCategory.Destroy;
begin
  FSection.Free;
  inherited;
end;

function TVkProductCategory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkProductCategory.FromJsonString(AJsonString: string): TVkProductCategory;
begin
  result := TJson.JsonToObject<TVkProductCategory>(AJsonString)
end;

{TVkMarket}

constructor TVkProduct.Create;
begin
  inherited;
  FCategory := TVkProductCategory.Create();
  FPrice := TVkProductPrice.Create();
end;

destructor TVkProduct.Destroy;
var
  LItemsItem: TVkPhoto;
begin
  for LItemsItem in FPhotos do
    LItemsItem.Free;
  FCategory.Free;
  FPrice.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  inherited;
end;

function TVkProduct.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkProduct.FromJsonString(AJsonString: string): TVkProduct;
begin
  result := TJson.JsonToObject<TVkProduct>(AJsonString)
end;

{TVkMarketAlbum}

constructor TVkMarketAlbum.Create;
begin
  inherited;
  FCategory := TVkProductCategory.Create();
  FPrice := TVkProductPrice.Create();
end;

destructor TVkMarketAlbum.Destroy;
begin
  FCategory.Free;
  FPrice.Free;
  inherited;
end;

function TVkMarketAlbum.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMarketAlbum.FromJsonString(AJsonString: string): TVkMarketAlbum;
begin
  result := TJson.JsonToObject<TVkMarketAlbum>(AJsonString)
end;

{ TVkMarkets }

procedure TVkProducts.Append(Audios: TVkProducts);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Audios.Items));
  Move(Audios.Items[0], FItems[OldLen], Length(Audios.Items) * SizeOf(TVkProduct));
end;

constructor TVkProducts.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkProducts.Destroy;
var
  LItemsItem: TVkProduct;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

class function TVkProducts.FromJsonString(AJsonString: string): TVkProducts;
begin
  result := TJson.JsonToObject<TVkProducts>(AJsonString);
end;

procedure TVkProducts.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

function TVkProducts.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

