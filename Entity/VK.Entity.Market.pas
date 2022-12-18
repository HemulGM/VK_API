unit VK.Entity.Market;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Photo, VK.Entity.Info, VK.Entity.Common, VK.Entity.Common.List,
  VK.Types, VK.Wrap.Interceptors;

type
  TVkMarketSection = class(TVkBasicObject)
  public
    /// <summary>
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Name;
  end;

  TVkProductPrice = class(TVkEntity)
  private
    FAmount: string;
    FCurrency: TVkCurrencyInfo;
    FText: string;
    FOld_amount: string;
    FOld_amount_text: string;
    FDiscount_rate: Integer;
    FAccess_key: string;
    FPrice_type: Integer;
    FPrice_unit: Integer;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ���� ������ � ����� ����� ������� ������
    /// </summary>
    property Amount: string read FAmount write FAmount;
    /// <summary>
    /// ������ ���� ������ � ����� ����� ������� ������
    /// </summary>
    property OldAmount: string read FOld_amount write FOld_amount;
    property OldAmountText: string read FOld_amount_text write FOld_amount_text;
    /// <summary>
    /// ������
    /// </summary>
    property Currency: TVkCurrencyInfo read FCurrency write FCurrency;
    /// <summary>
    /// ��������� ������������� ����
    /// </summary>
    property Text: string read FText write FText;
    property DiscountRate: Integer read FDiscount_rate write FDiscount_rate;
    property PriceType: Integer read FPrice_type write FPrice_type;
    property PriceUnit: Integer read FPrice_unit write FPrice_unit;
    destructor Destroy; override;
  end;

  TVkProductCategory = class(TVkBasicObject)
  private
    FSection: TVkMarketSection;
    FParent: TVkProductCategory;
  public
    /// <summary>
    /// ������������� ���������
    /// </summary>
    property Id;
    /// <summary>
    /// �������� ���������
    /// </summary>
    property Name;
    property Parent: TVkProductCategory read FParent write FParent;
    /// <summary>
    /// ������
    /// </summary>
    property Section: TVkMarketSection read FSection write FSection;
    destructor Destroy; override;
  end;

  TVkProductCategories = TVkEntityList<TVkProductCategory>;

  TVkDimensions = class(TVkEntity)
  private
    FWidth: Integer;
    FHeight: Integer;
    FLength: Integer;
  public
    /// <summary>
    /// ������ � �����������
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// ������ � �����������
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// ����� � �����������
    /// </summary>
    property Length: Integer read FLength write FLength;
  end;

  TVkProductPropertyValue = class(TVkEntity)
  private
    FVariant_id: Int64;
    FVariant_name: string;
    FProperty_name: string;
  public
    property VariantId: Int64 read FVariant_id write FVariant_id;
    property VariantName: string read FVariant_name write FVariant_name;
    property PropertyName: string read FProperty_name write FProperty_name;
  end;

  TVkProduct = class(TVkObject, IAttachment)
  private
    [JsonReflectAttribute(ctString, rtString, TProductAvailabilityInterceptor)]
    FAvailability: TVkProductAvailability;
    FCategory: TVkProductCategory;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDescription: string;
    FExternal_id: string;
    FOwner_id: TVkPeerId;
    FPrice: TVkProductPrice;
    FThumb_photo: string;
    FTitle: string;
    FPhotos: TArray<TVkPhoto>;
    FAlbums_ids: TArray<Int64>;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_comment: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
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
    FWeight: Integer;
    FSku: string;
    FUrl: string;
    FButton_title: string;
    FProperty_values: TArray<TVkProductPropertyValue>;
    FCsrf_hashes: string;
    FThumb: TVkSizes;
  public
    /// <summary>
    /// ������������� ������.
    /// </summary>
    property Id;
    /// <summary>
    /// ������������� ��������� ������.
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property AlbumsIds: TArray<Int64> read FAlbums_ids write FAlbums_ids;
    /// <summary>
    /// ������ ����������� ������
    /// </summary>
    property Availability: TVkProductAvailability read FAvailability write FAvailability;
    /// <summary>
    /// ����� �� ������ ������. ��������� ��������:
    /// ������
    /// ������� � �������
    /// ������ �����
    /// </summary>
    property ButtonTitle: string read FButton_title write FButton_title;
    /// <summary>
    /// ����������� �������������� ����� ��� �������� ������������
    /// </summary>
    property CanComment: Boolean read FCan_comment write FCan_comment;
    /// <summary>
    /// ����������� ������� ������ ������ ��� �������� ������������
    /// </summary>
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property CartQuantity: Integer read FCart_quantity write FCart_quantity;
    /// <summary>
    /// ��������� ������
    /// </summary>
    property Category: TVkProductCategory read FCategory write FCategory;
    /// <summary>
    /// ���� �������� ������
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// ����� �������� ������.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Dimensions: TVkDimensions read FDimensions write FDimensions;
    property ExternalId: string read FExternal_id write FExternal_id;
    /// <summary>
    /// True, ���� ������ �������� � �������� � �������� ������������
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsMainVariant: Boolean read FIs_main_variant write FIs_main_variant;
    /// <summary>
    /// ���������� �� �������� ���� ���������
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// ����������� ������. ������ ��������, ����������� ����������.
    /// </summary>
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    /// <summary>
    /// ����
    /// </summary>
    property Price: TVkProductPrice read FPrice write FPrice;
    property PropertyValues: TArray<TVkProductPropertyValue> read FProperty_values write FProperty_values;
    property Quantity: Integer read FQuantity write FQuantity;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// URL �����������-������� ������
    /// </summary>
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Title: string read FTitle write FTitle;
    property VariantsGroupingId: Integer read FVariants_grouping_id write FVariants_grouping_id;
    property ViewsCount: Integer read FViews_count write FViews_count;
    /// <summary>
    /// ��� � �������
    /// </summary>
    property Weight: Integer read FWeight write FWeight;
    /// <summary>
    /// ������� ������, ������������ ������ ������ �� 50 ��������
    /// </summary>
    property Sku: string read FSku write FSku;
    /// <summary>
    /// ������ �� ����� �� ������� ��������
    /// </summary>
    property Url: string read FUrl write FUrl;
    property CsrfHashes: string read FCsrf_hashes write FCsrf_hashes;
    property Thumb: TVkSizes read FThumb write FThumb;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkProducts = TVkEntityList<TVkProduct>;

implementation

uses
  VK.CommonUtils;

{TVkProductPrice}

destructor TVkProductPrice.Destroy;
begin
  if Assigned(FCurrency) then
    FCurrency.Free;
  inherited;
end;

{TVkProductCategory}

destructor TVkProductCategory.Destroy;
begin
  if Assigned(FSection) then
    FSection.Free;
  if Assigned(FParent) then
    FParent.Free;
  inherited;
end;

{TVkProduct}

destructor TVkProduct.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FPhotos);
  TArrayHelp.FreeArrayOfObject<TVkProductPropertyValue>(FProperty_values);
  if Assigned(FCategory) then
    FCategory.Free;
  if Assigned(FPrice) then
    FPrice.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FDimensions) then
    FDimensions.Free;
  TArrayHelp.FreeArrayOfObject<TVkSize>(FThumb);
  inherited;
end;

function TVkProduct.ToAttachment: TAttachment;
begin
  Result := TAttachment.Market(OwnerId, Id);
end;

end.

