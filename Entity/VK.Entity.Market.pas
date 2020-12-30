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

  TVkProductCategory = class(TVkObject)
  private
    FName: string;
    FSection: TVkMarketSection;
  public
    property Name: string read FName write FName;
    property Section: TVkMarketSection read FSection write FSection;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProductCategory;
  end;

  TVkProductCategories = class
  private
    FItems: TArray<TVkProductCategory>;
    FCount: Integer;
  public
    property Items: TArray<TVkProductCategory> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProductCategories;
  end;

  TVkProduct = class(TVkObject)
  private
    FAvailability: Boolean;
    FCategory: TVkProductCategory;
    FDate: Int64;
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
  public
    property Availability: Boolean read FAvailability write FAvailability;
    property Category: TVkProductCategory read FCategory write FCategory;
    property Date: Int64 read FDate write FDate;
    property Description: string read FDescription write FDescription;
    property ExternalId: string read FExternal_id write FExternal_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Price: TVkProductPrice read FPrice write FPrice;
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    property Title: string read FTitle write FTitle;
    property AlbumsIds: TArray<Integer> read FAlbums_ids write FAlbums_ids;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    //
    property CartQuantity: Integer read FCart_quantity write FCart_quantity;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsMainVariant: Boolean read FIs_main_variant write FIs_main_variant;
    property VariantsGroupingId: Integer read FVariants_grouping_id write FVariants_grouping_id;
    //Extended
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    property ViewsCount: Integer read FViews_count write FViews_count;
    //
    property Quantity: Integer read FQuantity write FQuantity;
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProducts;
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
begin
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FPhotos);
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

{ TVkProductCategories }

destructor TVkProductCategories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProductCategory>(FItems);
  inherited;
end;

class function TVkProductCategories.FromJsonString(AJsonString: string): TVkProductCategories;
begin
  result := TJson.JsonToObject<TVkProductCategories>(AJsonString);
end;

function TVkProductCategories.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

