unit VK.Entity.Market;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Photo, VK.Entity.Common;

type
  TVkMarketSection = TVkBasicObject;

  TVkProductPrice = class(TVkEntity)
  private
    FAmount: string;
    FCurrency: TVkProductCurrency;
    FText: string;
    FOld_amount: string;
    FOld_amount_text: string;
    FDiscount_rate: Integer;
  public
    property Amount: string read FAmount write FAmount;
    property OldAmount: string read FOld_amount write FOld_amount;
    property OldAmountText: string read FOld_amount_text write FOld_amount_text;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Text: string read FText write FText;
    property DiscountRate: Integer read FDiscount_rate write FDiscount_rate;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkProductCategory = class(TVkBasicObject)
  private
    FSection: TVkMarketSection;
  public
    property Section: TVkMarketSection read FSection write FSection;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkProductCategories = class(TVkEntity)
  private
    FItems: TArray<TVkProductCategory>;
    FCount: Integer;
  public
    property Items: TArray<TVkProductCategory> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
  end;

  TVkProduct = class(TVkObject)
  private
    FAvailability: Boolean;
    FCategory: TVkProductCategory;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDescription: string;
    FExternal_id: string;
    FOwner_id: Integer;
    FPrice: TVkProductPrice;
    FThumb_photo: string;
    FTitle: string;
    FPhotos: TArray<TVkPhoto>;
    FAlbums_ids: TArray<Integer>;
    FCan_comment: Boolean;
    FCan_repost: Boolean;
    FViews_count: Integer;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FIs_favorite: Boolean;
    FIs_main_variant: Boolean;
    FCart_quantity: Integer;
    FVariants_grouping_id: Integer;
    FQuantity: Integer;
    FDimensions: TVkDimensions;
  public
    property AlbumsIds: TArray<Integer> read FAlbums_ids write FAlbums_ids;
    property Availability: Boolean read FAvailability write FAvailability;
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property CartQuantity: Integer read FCart_quantity write FCart_quantity;
    property Category: TVkProductCategory read FCategory write FCategory;
    property Date: TDateTime read FDate write FDate;
    property Description: string read FDescription write FDescription;
    property Dimensions: TVkDimensions read FDimensions write FDimensions;
    property ExternalId: string read FExternal_id write FExternal_id;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsMainVariant: Boolean read FIs_main_variant write FIs_main_variant;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    property Price: TVkProductPrice read FPrice write FPrice;
    property Quantity: Integer read FQuantity write FQuantity;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    property Title: string read FTitle write FTitle;
    property VariantsGroupingId: Integer read FVariants_grouping_id write FVariants_grouping_id;
    property ViewsCount: Integer read FViews_count write FViews_count;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkProducts = class(TVkEntity)
  private
    FItems: TArray<TVkProduct>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkProduct> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

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

{TVkMarket}

constructor TVkProduct.Create;
begin
  inherited;
  FCategory := TVkProductCategory.Create();
  FPrice := TVkProductPrice.Create();
end;

destructor TVkProduct.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FPhotos);
  FCategory.Free;
  FPrice.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FDimensions) then
    FDimensions.Free;
  inherited;
end;

{ TVkMarkets }

constructor TVkProducts.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkProducts.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<TVkProduct>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

procedure TVkProducts.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkProductCategories }

destructor TVkProductCategories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProductCategory>(FItems);
  inherited;
end;

end.

