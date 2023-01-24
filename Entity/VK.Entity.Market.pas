unit VK.Entity.Market;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Photo,
  VK.Entity.Info, VK.Entity.Common, VK.Entity.Common.List, VK.Types,
  VK.Wrap.Interceptors;

type
  TVkMarketSection = class(TVkBasicObject)
  public
    /// <summary>
    /// Идентификатор секции
    /// </summary>
    property Id;
    /// <summary>
    /// Название секции
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
    /// Цена товара в сотых долях единицы валюты
    /// </summary>
    property Amount: string read FAmount write FAmount;
    /// <summary>
    /// Старая цена товара в сотых долях единицы валюты
    /// </summary>
    property OldAmount: string read FOld_amount write FOld_amount;
    property OldAmountText: string read FOld_amount_text write FOld_amount_text;
    /// <summary>
    /// Валюта
    /// </summary>
    property Currency: TVkCurrencyInfo read FCurrency write FCurrency;
    /// <summary>
    /// Строковое представление цены
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
    /// Идентификатор категории
    /// </summary>
    property Id;
    /// <summary>
    /// Название категории
    /// </summary>
    property Name;
    property Parent: TVkProductCategory read FParent write FParent;
    /// <summary>
    /// Секция
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
    /// Ширина в миллиметрах
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// Высота в миллиметрах
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// Длина в миллиметрах
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
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
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
    /// Идентификатор товара.
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор владельца товара.
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property AlbumsIds: TArray<Int64> read FAlbums_ids write FAlbums_ids;
    /// <summary>
    /// Статус доступности товара
    /// </summary>
    property Availability: TVkProductAvailability read FAvailability write FAvailability;
    /// <summary>
    /// Текст на кнопке товара. Возможные значения:
    /// Купить
    /// Перейти в магазин
    /// Купить билет
    /// </summary>
    property ButtonTitle: string read FButton_title write FButton_title;
    /// <summary>
    /// Возможность комментировать товар для текущего пользователя
    /// </summary>
    property CanComment: Boolean read FCan_comment write FCan_comment;
    /// <summary>
    /// Возможность сделать репост товара для текущего пользователя
    /// </summary>
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property CartQuantity: Integer read FCart_quantity write FCart_quantity;
    /// <summary>
    /// Категория товара
    /// </summary>
    property Category: TVkProductCategory read FCategory write FCategory;
    /// <summary>
    /// Дата создания товара
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Текст описания товара.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Габариты товара
    /// </summary>
    property Dimensions: TVkDimensions read FDimensions write FDimensions;
    property ExternalId: string read FExternal_id write FExternal_id;
    /// <summary>
    /// True, если объект добавлен в закладки у текущего пользователя
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsMainVariant: Boolean read FIs_main_variant write FIs_main_variant;
    /// <summary>
    /// Информация об отметках «Мне нравится»
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// Изображения товара. Массив объектов, описывающих фотографии.
    /// </summary>
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    /// <summary>
    /// Цена
    /// </summary>
    property Price: TVkProductPrice read FPrice write FPrice;
    property PropertyValues: TArray<TVkProductPropertyValue> read FProperty_values write FProperty_values;
    property Quantity: Integer read FQuantity write FQuantity;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// URL изображения-обложки товара
    /// </summary>
    property ThumbPhoto: string read FThumb_photo write FThumb_photo;
    /// <summary>
    /// Название товара
    /// </summary>
    property Title: string read FTitle write FTitle;
    property VariantsGroupingId: Integer read FVariants_grouping_id write FVariants_grouping_id;
    property ViewsCount: Integer read FViews_count write FViews_count;
    /// <summary>
    /// Вес в граммах
    /// </summary>
    property Weight: Integer read FWeight write FWeight;
    /// <summary>
    /// Артикул товара, произвольная строка длиной до 50 символов
    /// </summary>
    property Sku: string read FSku write FSku;
    /// <summary>
    /// Ссылка на товар во внешних ресурсах
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

