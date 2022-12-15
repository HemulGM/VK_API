unit VK.Entity.Group;

interface

uses
  System.SysUtils, Generics.Collections, Rest.Json, VK.Entity.Common, VK.Types,
  VK.Entity.Photo, REST.JsonReflect, REST.Json.Interceptors, VK.Entity.Market,
  VK.Entity.Group.Counters, VK.Wrap.Interceptors, VK.Entity.Database.Cities,
  VK.Entity.Database.Countries, VK.Entity.Common.List, VK.Entity.Geo;

type
  TVkGroupSubject = TVkBasicObject;

  TVkCoverImages = TVkEntityList<TVkSize>;

  TVkCover = class
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEnabled: Boolean;
    FImages: TArray<TVkSize>;
  public
    /// <summary>
    /// Информация о том, включена ли обложка
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// Копии изображений обложки
    /// </summary>
    property Images: TArray<TVkSize> read FImages write FImages;
    destructor Destroy; override;
  end;

  TVkGroupLink = class(TVkBasicObject)
  private
    FPhoto_50: string;
    FUrl: string;
    FDesc: string;
    FPhoto_100: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEdit_title: Boolean;
    FImage_processing: Integer;
  public
    /// <summary>
    /// Идентификатор ссылки
    /// </summary>
    property Id;
    /// <summary>
    /// URL
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// Название ссылки
    /// </summary>
    property Name;
    /// <summary>
    /// Описание ссылки
    /// </summary>
    property Desc: string read FDesc write FDesc;
    /// <summary>
    /// URL изображения-превью шириной 50px
    /// </summary>
    property Photo_50: string read FPhoto_50 write FPhoto_50;
    /// <summary>
    /// URL изображения-превью шириной 100px
    /// </summary>
    property Photo_100: string read FPhoto_100 write FPhoto_100;
    property EditTitle: Boolean read FEdit_title write FEdit_title;
    property ImageProcessing: Integer read FImage_processing write FImage_processing;
  end;

  TVkGroupMemberState = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FMember: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_invite: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRequest: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FInvitation: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_recall: Boolean;
    FUser_id: Integer;
  public
    /// <summary>
    /// Является ли пользователь участником сообщества
    /// </summary>
    property Member: Boolean read FMember write FMember;
    /// <summary>
    /// Есть ли непринятая заявка от пользователя на вступление в группу (такую заявку можно отозвать методом Groups.Leave)
    /// </summary>
    property Request: Boolean read FRequest write FRequest;
    /// <summary>
    /// Приглашён ли пользователь в группу или встречу
    /// </summary>
    property Invitation: Boolean read FInvitation write FInvitation;
    /// <summary>
    /// Может ли автор запроса приглашать пользователя в группу
    /// </summary>
    property CanInvite: Boolean read FCan_invite write FCan_invite;
    /// <summary>
    /// Может ли автор отменить приглашение. Появляется, если Invitation: True
    /// </summary>
    property CanRecall: Boolean read FCan_recall write FCan_recall;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
  end;

  TVkGroupMemberStates = TVkEntityList<TVkGroupMemberState>;

  TVkGroupMarket = class(TVkObject)
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEnabled: Boolean;
    FMain_album_id: Integer;
    FPrice_min: Integer;
    FCurrency_text: string;
    FPrice_max: Integer;
    FContact_id: Integer;
    FCurrency: TVkProductCurrency;
    FType: string;
  public
    /// <summary>
    /// Информация о том, включен ли блок товаров в сообществе
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// Минимальная цена товаров
    /// </summary>
    property PriceMin: Integer read FPrice_min write FPrice_min;
    /// <summary>
    /// Максимальная цена товаров
    /// </summary>
    property PriceMax: Integer read FPrice_max write FPrice_max;
    /// <summary>
    /// Идентификатор главной подборки товаров
    /// </summary>
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    /// <summary>
    /// Идентификатор контактного лица для связи с продавцом. Возвращается отрицательное значение, если для связи с продавцом используются сообщения сообщества
    /// </summary>
    property ContactId: Integer read FContact_id write FContact_id;
    /// <summary>
    /// Информация о валюте
    /// </summary>
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Name: string read FName write FName;
    /// <summary>
    /// Информация о типе магазина (basic и advanced)
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Строковое обозначение
    /// </summary>
    property CurrencyText: string read FCurrency_text write FCurrency_text;
    destructor Destroy; override;
  end;

  TVkGroupAddress = class(TVkObject)
  private
    FAdditional_address: string;
    FAddress: string;
    FCity_id: Integer;
    FCountry_id: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FMetro_station_id: Integer;
    FTime_offset: Integer;
    FTitle: string;
    FWork_info_status: string;
  public
    property Additional_address: string read FAdditional_address write FAdditional_address;
    property Address: string read FAddress write FAddress;
    property CityId: Integer read FCity_id write FCity_id;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property MetroStationId: Integer read FMetro_station_id write FMetro_station_id;
    property TimeOffset: Integer read FTime_offset write FTime_offset;
    property Title: string read FTitle write FTitle;
    property WorkInfoStatus: string read FWork_info_status write FWork_info_status;
  end;

  TVkGroupAddresses = TVkEntityList<TVkGroupAddress>;

  TVkBanInfo = class
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FComment: string;
  public
    /// <summary>
    /// Срок окончания блокировки
    /// </summary>
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    /// <summary>
    /// Комментарий к блокировке
    /// </summary>
    property Comment: string read FComment write FComment;
  end;

  TVkContact = class
  private
    FEmail: string;
    FPhone: string;
    FDesc: string;
    FUser_id: Integer;
  public
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
    /// <summary>
    /// Должность
    /// </summary>
    property Desc: string read FDesc write FDesc;
    /// <summary>
    /// Номер телефона
    /// </summary>
    property Phone: string read FPhone write FPhone;
    /// <summary>
    /// Адрес e-mail
    /// </summary>
    property Email: string read FEmail write FEmail;
  end;

  TVkAddresses = class
  private
    FIs_enabled: Boolean;
    FMain_address_id: Integer;
  public
    /// <summary>
    /// Включен ли блок адресов в сообществе
    /// </summary>
    property IsEnabled: Boolean read FIs_enabled write FIs_enabled;
    /// <summary>
    /// Идентификатор основного адреса
    /// </summary>
    property MainAddressId: Integer read FMain_address_id write FMain_address_id;
  end;

  TVkLikeFriends = class
  private
    FCount: Integer;
    FPreview: TArray<TVkPeerId>;
  public
    property Count: Integer read FCount write FCount;
    property Preview: TArray<TVkPeerId> read FPreview write FPreview;
  end;

  TVkLike = class
  private
    FIs_liked: Boolean;
    FFriends: TVkLikeFriends;
  public
    property IsLiked: Boolean read FIs_liked write FIs_liked;
    property Friends: TVkLikeFriends read FFriends write FFriends;
    destructor Destroy; override;
  end;

  TVkGroup = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TGroupAdminLevelInterceptor)]
    FAdmin_level: TVkGroupAdminLevel;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_admin: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_advertiser: Boolean;
    [JsonReflectAttribute(ctString, rtString, TGroupAccessInterceptor)]
    FIs_closed: TVkGroupAccess;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_member: Boolean;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    [JsonReflectAttribute(ctString, rtString, TGroupTypeInterceptor)]
    FType: TVkGroupType;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FTrending: Boolean;
    FMembers_count: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FVerified: Boolean;
    FCountry: TVkCountry;
    FMember_status: Integer;
    FCity: TVkCity;
    FActivity: string;
    [JsonReflectAttribute(ctString, rtString, TDeactivatedInterceptor)]
    FDeactivated: TVkDeactivated;
    FInvited_by: Integer;
    FAddresses: TVkAddresses;
    [JsonReflectAttribute(ctString, rtString, TAgeLimitsInterceptor)]
    FAge_limits: TVkAgeLimits;
    FBan_info: TVkBanInfo;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_create_topic: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_message: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_post: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_see_all_posts: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_upload_doc: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_upload_video: Boolean;
    FContacts: TArray<TVkContact>;
    FCover: TVkCover;
    FCrop_photo: TVkCropPhoto;
    FDescription: string;
    FFixed_post: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FHas_photo: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_favorite: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_hidden_from_feed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_messages_blocked: Boolean;
    FLinks: TArray<TVkGroupLink>;
    FMain_album_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TGroupMainSectionInterceptor)]
    FMain_section: TVkGroupMainSection;
    FMarket: TVkGroupMarket;
    FPlace: TVkPlace;
    FPublic_date_label: string;
    FSite: string;
    FStatus: string;
    FWall: Integer;
    FWiki_page: string;
    FCounters: TVkGroupCounters;
    FTrack_code: string;
    [JsonReflectAttribute(ctString, rtString, TIntDateTimeInterceptor)]
    FStart_date: TDateTime;
    FLike: TVkLike;
    function GetFixedPostId: string;
  public
    /// <summary>
    /// Строка тематики паблика. У групп возвращается строковое значение, открыта ли группа или нет, а у событий дата начала
    /// </summary>
    property Activity: string read FActivity write FActivity;
    /// <summary>
    /// Уровень полномочий текущего пользователя (если IsAdmin = True)
    /// </summary>
    property AdminLevel: TVkGroupAdminLevel read FAdmin_level write FAdmin_level;
    /// <summary>
    /// Информация об адресах сообщества
    /// </summary>
    property Addresses: TVkAddresses read FAddresses write FAddresses;
    /// <summary>
    /// Возрастное ограничение
    /// </summary>
    property AgeLimits: TVkAgeLimits read FAge_limits write FAge_limits;
    /// <summary>
    /// Информация о занесении в черный список сообщества (поле возвращается только при запросе информации об одном сообществе)
    /// </summary>
    property BanInfo: TVkBanInfo read FBan_info write FBan_info;
    /// <summary>
    /// Информация о том, может ли текущий пользователь создать новое обсуждение в группе
    /// </summary>
    property CanCreateTopic: Boolean read FCan_create_topic write FCan_create_topic;
    /// <summary>
    /// Информация о том, может ли текущий пользователь написать сообщение сообществу
    /// </summary>
    property CanMessage: Boolean read FCan_message write FCan_message;
    /// <summary>
    /// Информация о том, может ли текущий пользователь оставлять записи на стене сообщества
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_message;
    /// <summary>
    /// Информация о том, разрешено ли видеть чужие записи на стене группы
    /// </summary>
    property CanSeeAllPosts: Boolean read FCan_see_all_posts write FCan_see_all_posts;
    /// <summary>
    /// Информация о том, может ли текущий пользователь загружать документы в группу
    /// </summary>
    property CanUploadDoc: Boolean read FCan_upload_doc write FCan_upload_doc;
    /// <summary>
    /// Информация о том, может ли текущий пользователь загружать видеозаписи в группу
    /// </summary>
    property CanUploadVideo: Boolean read FCan_upload_video write FCan_upload_video;
    /// <summary>
    /// Город, указанный в информации о сообществе
    /// </summary>
    property City: TVkCity read FCity write FCity;
    /// <summary>
    /// Информация из блока контактов публичной страницы
    /// </summary>
    property Contacts: TArray<TVkContact> read FContacts write FContacts;
    /// <summary>
    /// Объект, содержащий счётчики сообщества, может включать любой набор из следующих полей: photos, albums, audios, videos, topics, docs.
    /// </summary>
    property Counters: TVkGroupCounters read FCounters write FCounters;
    /// <summary>
    /// Страна, указанная в информации о сообществе.
    /// </summary>
    property Country: TVkCountry read FCountry write FCountry;
    /// <summary>
    /// Обложка сообщества.
    /// </summary>
    property Cover: TVkCover read FCover write FCover;
    /// <summary>
    /// Возвращает данные о точках, по которым вырезаны профильная и миниатюрная фотографии сообщества.
    /// </summary>
    property CropPhoto: TVkCropPhoto read FCrop_photo write FCrop_photo;
    /// <summary>
    /// Возвращается в случае, если сообщество удалено или заблокировано
    /// </summary>
    property Deactivated: TVkDeactivated read FDeactivated write FDeactivated;
    /// <summary>
    /// Текст описания сообщества.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Идентификатор закрепленной записи. Получить дополнительные данные о записи можно методом wall.getById, передав в поле posts {group_id}_{post_id}.
    /// </summary>
    property FixedPost: Integer read FFixed_post write FFixed_post;
    /// <summary>
    /// Идентификатор закрепленной записи. Получить дополнительные данные о записи можно методом wall.getById, передав в поле posts.
    /// </summary>
    property FixedPostId: string read GetFixedPostId;
    /// <summary>
    /// Информация о том, установлена ли у сообщества главная фотография.
    /// </summary>
    property HasPhoto: Boolean read FHas_photo write FHas_photo;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор пользователя, который отправил приглашение в сообщество.
    /// Поле возвращается только для метода groups.getInvites.
    /// </summary>
    property InvitedBy: Integer read FInvited_by write FInvited_by;
    /// <summary>
    /// Информация о том, является ли текущий пользователь руководителем.
    /// </summary>
    property IsAdmin: Boolean read FIs_admin write FIs_admin;
    /// <summary>
    /// Информация о том, является ли текущий пользователь рекламодателем.
    /// </summary>
    property IsAdvertiser: Boolean read FIs_advertiser write FIs_advertiser;
    /// <summary>
    /// Является ли сообщество закрытым
    /// </summary>
    property IsClosed: TVkGroupAccess read FIs_closed write FIs_closed;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property IsMessagesBlocked: Boolean read FIs_messages_blocked write FIs_messages_blocked;
    property IsMember: Boolean read FIs_member write FIs_member;
    property Like: TVkLike read FLike write FLike;
    /// <summary>
    /// Информация из блока ссылок сообщества
    /// </summary>
    property Links: TArray<TVkGroupLink> read FLinks write FLinks;
    /// <summary>
    /// Идентификатор основного фотоальбома
    /// </summary>
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    /// <summary>
    /// Информация о главной секции
    /// </summary>
    property MainSection: TVkGroupMainSection read FMain_section write FMain_section;
    /// <summary>
    /// Информация о магазине
    /// </summary>
    property Market: TVkGroupMarket read FMarket write FMarket;
    /// <summary>
    /// Количество участников в сообществе
    /// </summary>
    property MembersCount: Integer read FMembers_count write FMembers_count;
    { TODO -oHemulGM -c : Сделать тип 16.02.2021 13:57:27 }
    /// <summary>
    /// Статус участника текущего пользователя
    /// </summary>
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// Место, указанное в информации о сообществе
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    property PublicDateLabel: string read FPublic_date_label write FPublic_date_label;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Site: string read FSite write FSite;
    property StartDate: TDateTime read FStart_date write FStart_date;
    property Status: string read FStatus write FStatus;
    property &Type: TVkGroupType read FType write FType;
    property Trending: Boolean read FTrending write FTrending;
    property Verified: Boolean read FVerified write FVerified;
    property Wall: Integer read FWall write FWall;
    property WikiPage: string read FWiki_page write FWiki_page;
    destructor Destroy; override;
  end;

  TVkGroups = TVkObjectList<TVkGroup>;

  TVkGroupTag = class(TVkObject)
  private
    FColor: string;
    FName: string;
  public
    property Color: string read FColor write FColor;
    property Id;
    property Name: string read FName write FName;
  end;

  TVkGroupTags = TVkObjectList<TVkGroupTag>;

implementation

uses
  VK.CommonUtils;

{TVkGroup}

destructor TVkGroup.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkContact>(FContacts);
  TArrayHelp.FreeArrayOfObject<TVkGroupLink>(FLinks);
  if Assigned(FCity) then
    FCity.Free;
  if Assigned(FCountry) then
    FCountry.Free;
  if Assigned(FAddresses) then
    FAddresses.Free;
  if Assigned(FBan_info) then
    FBan_info.Free;
  if Assigned(FCover) then
    FCover.Free;
  if Assigned(FCounters) then
    FCounters.Free;
  if Assigned(FCrop_photo) then
    FCrop_photo.Free;
  if Assigned(FMarket) then
    FMarket.Free;
  if Assigned(FPlace) then
    FPlace.Free;
  if Assigned(FLike) then
    FLike.Free;
  inherited;
end;

function TVkGroup.GetFixedPostId: string;
begin
  //{group_id}_{post_id}
  Result := FId.ToString + '_' + FFixed_post.ToString;
end;

{ TVkGroupMarket }

destructor TVkGroupMarket.Destroy;
begin
  if Assigned(FCurrency) then
    FCurrency.Free;
  inherited;
end;

{ TVkCover }

destructor TVkCover.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FImages);
  inherited;
end;

{ TVkLike }

destructor TVkLike.Destroy;
begin
  if Assigned(FFriends) then
    FFriends.Free;
  inherited;
end;

end.

