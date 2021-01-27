unit VK.Entity.Market;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Photo, VK.Entity.Info, VK.Entity.Common, VK.Entity.Common.List,
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
    FCurrency: TVkProductCurrency;
    FText: string;
    FOld_amount: string;
    FOld_amount_text: string;
    FDiscount_rate: Integer;
    FAccess_key: string;
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
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    /// <summary>
    /// Строковое представление цены
    /// </summary>
    property Text: string read FText write FText;
    property DiscountRate: Integer read FDiscount_rate write FDiscount_rate;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkProductCategory = class(TVkBasicObject)
  private
    FSection: TVkMarketSection;
  public
    /// <summary>
    /// Идентификатор категории
    /// </summary>
    property Id;
    /// <summary>
    /// Название категории
    /// </summary>
    property Name;
    /// <summary>
    /// Секция
    /// </summary>
    property Section: TVkMarketSection read FSection write FSection;
    constructor Create; override;
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

  TVkProduct = class(TVkObject)
  private
    {
        0 — товар доступен;
        1 — товар удален;
        2 — товар недоступен.
    }
    FAvailability: Integer;
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
  public
    /// <summary>
    /// Идентификатор товара.
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор владельца товара.
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property AlbumsIds: TArray<Integer> read FAlbums_ids write FAlbums_ids;
    /// <summary>
    /// Статус доступности товара
    /// </summary>
    property Availability: Integer read FAvailability write FAvailability;
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
    destructor Destroy; override;
  end;

  TVkProducts = TVkEntityList<TVkProduct>;

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

destructor TVkProduct.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FPhotos);
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
  inherited;
end;

end.

