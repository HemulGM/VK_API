unit VK.Entity.Link;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Photo, VK.Entity.Market,
  VK.Entity.App, VK.Types, VK.Entity.Attachment;

type
  TVkLinkStatus = class(TVkEntity)
  private
    FLink: string;
    FStatus: string;
    function GetStatus: TVkLinkStatusType;
    procedure SetStatus(const Value: TVkLinkStatusType);
  public
    property Link: string read FLink write FLink;
    property Status: TVkLinkStatusType read GetStatus write SetStatus;
  end;

  TVkLinkAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TVkLinkButton = class(TVkEntity)
  private
    FAction: TVkLinkAction;
    FTitle: string;
  public
    property Action: TVkLinkAction read FAction write FAction;
    property Title: string read FTitle write FTitle;
    constructor Create; override;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Объект rating, описывающий информацию о рейтинге продукта
  /// </summary>
  TVkRating = class
  private
    FReviews_count: Integer;
    FStars: Integer;
  public
    property Stars: Integer read FStars write FStars;
    property ReviewsCount: Integer read FReviews_count write FReviews_count;
  end;

  /// <summary>
  /// Прикрепленная ссылка
  /// </summary>
  TVkLink = class(TVkEntity, IAttachment)
  private
    FButton: TVkLinkButton;
    FCaption: string;
    FDescription: string;
    FPhoto: TVkPhoto;
    FTitle: string;
    FUrl: string;
    FText: string;
    FAccess_key: string;
    FProduct: TVkProduct;
    FPreview_page: string;
    FPreview_url: string;
    FImage_src: string;
    FApplication: TVkStoreApplication;
    FIs_external: Boolean;
    FRating: TVkRating;
  public
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Объект application (если имеется)
    /// </summary>
    property Application: TVkStoreApplication read FApplication write FApplication;
    /// <summary>
    /// Информация о кнопке для перехода (если имеется)
    /// </summary>
    property Button: TVkLinkButton read FButton write FButton;
    /// <summary>
    /// Подпись ссылки (если имеется)
    /// </summary>
    property Caption: string read FCaption write FCaption;
    /// <summary>
    /// Описание ссылки
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// URL изображения для превью ссылки. (Для версий API ниже 5.37)
    /// </summary>
    property ImageSrc: string read FImage_src write FImage_src;
    /// <summary>
    /// Является ли ссылкой на внешний ресурс (если имеется)
    /// </summary>
    property IsExternal: Boolean read FIs_external write FIs_external;
    /// <summary>
    /// Изображение превью, объект фотографии (если имеется)
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Идентификатор вики-страницы с контентом для предпросмотра содержимого страницы. Возвращается в формате "owner_id_page_id".
    /// </summary>
    property PreviewPage: string read FPreview_page write FPreview_page;
    /// <summary>
    /// URL страницы с контентом для предпросмотра содержимого страницы
    /// </summary>
    property PreviewUrl: string read FPreview_url write FPreview_url;
    /// <summary>
    /// Информация о продукте (если имеется). Поле возвращается для ссылок на магазины, например, Aliexpress. Объект с единственным полем price (object)
    /// </summary>
    property Product: TVkProduct read FProduct write FProduct;
    /// <summary>
    /// Информацию о рейтинге продукта
    /// </summary>
    property Rating: TVkRating read FRating write FRating;
    /// <summary>
    /// Заголовок ссылки
    /// </summary>
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    /// <summary>
    /// URL ссылки
    /// </summary>
    property Url: string read FUrl write FUrl;
    destructor Destroy; override;
    function ToAttachment: string;
  end;

  TVkShortLink = class(TVkEntity)
  private
    FKey: string;
    FShort_url: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTimestamp: TDateTime;
    FUrl: string;
    FViews: Integer;
    FAccess_key: string;
  public
    property Key: string read FKey write FKey;
    property AccessKey: string read FAccess_key write FAccess_key;
    property ShortUrl: string read FShort_url write FShort_url;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Url: string read FUrl write FUrl;
    property Views: Integer read FViews write FViews;
  end;

  TVkShortLinks = TVkEntityList<TVkShortLink>;

  TVkLinkViewsCity = class
  private
    FCity_id: Integer;
    FViews: Integer;
  public
    property CityId: Integer read FCity_id write FCity_id;
    property Views: Integer read FViews write FViews;
  end;

  TVkLinkViewsCountries = class
  private
    FCountry_id: Integer;
    FViews: Integer;
  public
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Views: Integer read FViews write FViews;
  end;

  TVkLinkSexAge = class
  private
    FAge_range: string;
    FFemale: Integer;
    FMale: Integer;
  public
    property AgeRange: string read FAge_range write FAge_range;
    property Female: Integer read FFemale write FFemale;
    property Male: Integer read FMale write FMale;
  end;

  TVkLinkStats = class(TVkEntity)
  private
    FCities: TArray<TVkLinkViewsCity>;
    FCountries: TArray<TVkLinkViewsCountries>;
    FSex_age: TArray<TVkLinkSexAge>;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTimestamp: TDateTime;
    FViews: Integer;
  public
    property Cities: TArray<TVkLinkViewsCity> read FCities write FCities;
    property Countries: TArray<TVkLinkViewsCountries> read FCountries write FCountries;
    property SexAge: TArray<TVkLinkSexAge> read FSex_age write FSex_age;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Views: Integer read FViews write FViews;
    destructor Destroy; override;
  end;

  TVkLinkStates = class
  private
    FKey: string;
    FStats: TArray<TVkLinkStats>;
  public
    property Key: string read FKey write FKey;
    property Stats: TArray<TVkLinkStats> read FStats write FStats;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils, System.StrUtils;

{TButtonClass}

constructor TVkLinkButton.Create;
begin
  inherited;
  FAction := TVkLinkAction.Create();
end;

destructor TVkLinkButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

{TLinkClass}

destructor TVkLink.Destroy;
begin
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FButton) then
    FButton.Free;
  if Assigned(FProduct) then
    FProduct.Free;
  if Assigned(FApplication) then
    FApplication.Free;
  if Assigned(FRating) then
    FRating.Free;
  inherited;
end;

{ TVkLinkStatus }

function TVkLinkStatus.GetStatus: TVkLinkStatusType;
begin
  Result := TVkLinkStatusType.FromString(FStatus);
end;

procedure TVkLinkStatus.SetStatus(const Value: TVkLinkStatusType);
begin
  FStatus := Value.ToString;
end;

{ TVkLinkStats }

destructor TVkLinkStats.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkLinkViewsCity>(FCities);
  TArrayHelp.FreeArrayOfObject<TVkLinkViewsCountries>(FCountries);
  TArrayHelp.FreeArrayOfObject<TVkLinkSexAge>(FSex_age);
  inherited;
end;

{ TVkLinkStates }

destructor TVkLinkStates.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkLinkStats>(FStats);
  inherited;
end;

function TVkLink.ToAttachment: string;
begin
  { TODO -oМалинин Геннадий -c : Додеать вложение ссылки 26.01.2021 12:23:45 }
  Result := Attachment.Link(0, 0, AccessKey);
end;

end.

