unit VK.Entity.Group;

interface

uses
  System.SysUtils, Generics.Collections, Rest.Json, VK.Entity.Common,
  VK.Entity.Photo, VK.Entity.Market, VK.Entity.Group.Counters,
  VK.Entity.Database.Cities, VK.Entity.Database.Countries, VK.Entity.Common.List;

type
  TVkGroupStatusType = (gsNone, gsOnline, gsAnswerMark);

  TVkGroupSubject = TVkBasicObject;

  TVkGroupStatus = class(TVkEntity)
  private
    FMinutes: Integer;
    FStatus: string;
    function GetStatus: TVkGroupStatusType;
  public
    /// <summary>
    /// Оценка времени ответа в минутах (для status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    /// <returns>none — сообщество не онлайн; online — сообщество онлайн (отвечает мгновенно); answer_mark — сообщество отвечает быстро.</returns>
    property StatusStr: string read FStatus write FStatus;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    property Status: TVkGroupStatusType read GetStatus;
  end;

  TVkCoverImages = TVkEntityList<TVkImage>;

  TVkCover = class
  private
    FEnabled: Boolean;
    FImages: TArray<TVkImage>;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property Images: TArray<TVkImage> read FImages write FImages;
  end;

  TVkGroupLink = class(TVkBasicObject)
  private
    FPhoto_50: string;
    FUrl: string;
    FDesc: string;
    FPhoto_100: string;
    FEdit_title: Integer;
    FImage_processing: Integer;
  public
    property Url: string read FUrl write FUrl;
    property Desc: string read FDesc write FDesc;
    property Photo_50: string read FPhoto_50 write FPhoto_50;
    property Photo_100: string read FPhoto_100 write FPhoto_100;
    property EditTitle: Integer read FEdit_title write FEdit_title;
    property ImageProcessing: Integer read FImage_processing write FImage_processing;
  end;

  TVkGroupMemberState = class(TVkEntity)
  private
    FMember: Boolean;
    FUser_id: Integer;
    FCan_invite: Boolean;
    FRequest: Boolean;
    FInvitation: Boolean;
    FCan_recall: Boolean;
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
    FEnabled: Integer;
    FMain_album_id: Integer;
    FPrice_min: Integer;
    FCurrency_text: string;
    FPrice_max: Integer;
    FContact_id: Integer;
    FCurrency: TVkProductCurrency;
  public
    property Enabled: Integer read FEnabled write FEnabled;
    property PriceMin: Integer read FPrice_min write FPrice_min;
    property PriceMax: Integer read FPrice_max write FPrice_max;
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    property ContactId: Integer read FContact_id write FContact_id;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Name: string read FName write FName;
    property Currency_text: string read FCurrency_text write FCurrency_text;
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

  TVkGroupState = (gsOpen = 0, gsClose = 1, gsPrivate = 2);

  TVkGroup = class(TVkObject)
  private
    FAdmin_level: Integer;
    FIs_admin: Boolean;
    FIs_advertiser: Boolean;
    FIs_closed: Integer;
    FIs_member: Boolean;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FTrending: Boolean;
    FMembers_count: Integer;
    FVerified: Boolean;
    FCountry: TVkCountry;
    FMember_status: Integer;
    FCity: TVkCity;
    FActivity: string;
    FDeactivated: string;
    FInvited_by: Integer;
    FAddresses: TVkAddresses;
    FAge_limits: Integer;
    FBan_info: TVkBanInfo;
    Fcan_create_topic: Boolean;
    Fcan_message: Boolean;
    Fcan_post: Boolean;
    Fcan_see_all_posts: Boolean;
    Fcan_upload_doc: Boolean;
    Fcan_upload_video: Boolean;
    FContacts: TArray<TVkContact>;
    FCover: TVkCover;
    FCrop_photo: TVkCropPhoto;
    FDescription: string;
    FFixed_post: Integer;
    FHas_photo: Boolean;
    FIs_favorite: Boolean;
    FIs_hidden_from_feed: Boolean;
    FIs_messages_blocked: Boolean;
    FLinks: TArray<TVkGroupLink>;
    FMain_album_id: Integer;
    FMain_section: Integer;
    FMarket: TVkGroupMarket;
    FPlace: TVkPlace;
    FPublic_date_label: string;
    FSite: string;
    FStatus: string;
    FWall: Integer;
    FWiki_page: string;
    FCounters: TVkGroupCounters;
    FTrack_code: string;
    function GetIsBanned: Boolean;
    function GetIsDeleted: Boolean;
    function GetIsDeactivated: Boolean;
    function GetFixedPostId: string;
  public
    /// <summary>
    /// Строка тематики паблика. У групп возвращается строковое значение, открыта ли группа или нет, а у событий дата начала.
    /// </summary>
    property Activity: string read FActivity write FActivity;
    /// <summary>
    /// Уровень полномочий текущего пользователя (если is_admin = 1):
    /// 1 — модератор;
    /// 2 — редактор;
    /// 3 — администратор.
    /// </summary>
    property AdminLevel: Integer read FAdmin_level write FAdmin_level;
    /// <summary>
    /// Информация об адресах сообщества.
    /// </summary>
    property Addresses: TVkAddresses read FAddresses write FAddresses;
    /// <summary>
    /// Возрастное ограничение.
    /// 1 — нет; 2 — 16+; 3 — 18+.
    /// </summary>
    property AgeLimits: Integer read FAge_limits write FAge_limits;
    /// <summary>
    /// Информация о занесении в черный список сообщества (поле возвращается только при запросе информации об одном сообществе)
    /// </summary>
    property BanInfo: TVkBanInfo read FBan_info write FBan_info;
    /// <summary>
    /// Информация о том, может ли текущий пользователь создать новое обсуждение в группе.
    /// </summary>
    property CanCreateTopic: Boolean read Fcan_create_topic write Fcan_create_topic;
    /// <summary>
    /// Информация о том, может ли текущий пользователь написать сообщение сообществу.
    /// </summary>
    property CanMessage: Boolean read Fcan_message write Fcan_message;
    /// <summary>
    /// Информация о том, может ли текущий пользователь оставлять записи на стене сообщества.
    /// </summary>
    property CanPost: Boolean read Fcan_post write Fcan_post;
    /// <summary>
    /// Информация о том, разрешено ли видеть чужие записи на стене группы.
    /// </summary>
    property CanSeeAllPosts: Boolean read Fcan_see_all_posts write Fcan_see_all_posts;
    /// <summary>
    /// Информация о том, может ли текущий пользователь загружать документы в группу.
    /// </summary>
    property CanUploadDoc: Boolean read Fcan_upload_doc write Fcan_upload_doc;
    /// <summary>
    /// Информация о том, может ли текущий пользователь загружать видеозаписи в группу.
    /// </summary>
    property CanUploadVideo: Boolean read Fcan_upload_video write Fcan_upload_video;
    /// <summary>
    /// Город, указанный в информации о сообществе.
    /// </summary>
    property City: TVkCity read FCity write FCity;
    /// <summary>
    /// Информация из блока контактов публичной страницы.
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
    /// Возвращается в случае, если сообщество удалено или заблокировано. Возможные значения:
    /// deleted — сообщество удалено;
    /// banned — сообщество заблокировано;
    /// </summary>
    property Deactivated: string read FDeactivated write FDeactivated;
    /// <summary>
    /// Сообщество удалено
    /// </summary>
    property IsDeleted: Boolean read GetIsDeleted;
    /// <summary>
    /// Сообщество заблокировано
    /// </summary>
    property IsBanned: Boolean read GetIsBanned;
    /// <summary>
    /// Сообщество не активно
    /// </summary>
    property IsDeactivated: Boolean read GetIsDeactivated;
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
    /// Является ли сообщество закрытым. Возможные значения:
    /// 0 — открытое;
    /// 1 — закрытое;
    /// 2 — частное.
    /// </summary>
    property IsClosed: Integer read FIs_closed write FIs_closed;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property IsMessagesBlocked: Boolean read FIs_messages_blocked write FIs_messages_blocked;
    property IsMember: Boolean read FIs_member write FIs_member;
    property Links: TArray<TVkGroupLink> read FLinks write FLinks;
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    property MainSection: Integer read FMain_section write FMain_section;
    property Market: TVkGroupMarket read FMarket write FMarket;
    property MembersCount: Integer read FMembers_count write FMembers_count;
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property TrackCode: string read FTrack_code write FTrack_code;
    property Place: TVkPlace read FPlace write FPlace;
    property PublicDateLabel: string read FPublic_date_label write FPublic_date_label;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Site: string read FSite write FSite;
    property Status: string read FStatus write FStatus;
    property&Type: string read FType write FType;
    property Trending: Boolean read FTrending write FTrending;
    property Verified: Boolean read FVerified write FVerified;
    property Wall: Integer read FWall write FWall;
    property WikiPage: string read FWiki_page write FWiki_page;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGroups = TVkEntityList<TVkGroup>;

  TVkGroupTag = class(TVkObject)
  private
    FColor: string;
    FName: string;
  public
    property Color: string read FColor write FColor;
    property Id;
    property Name: string read FName write FName;
  end;

  TVkGroupTags = TVkEntityList<TVkGroupTag>;

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;

implementation

uses
  VK.CommonUtils;

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(List) to High(List) do
    if List[i].Id = Abs(Id) then
      Exit(i);
end;

{TVkGroup}

constructor TVkGroup.Create;
begin
  inherited;
end;

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
  inherited;
end;

function TVkGroup.GetFixedPostId: string;
begin
  //{group_id}_{post_id}
  Result := FId.ToString + '_' + FFixed_post.ToString;
end;

function TVkGroup.GetIsBanned: Boolean;
begin
  Result := FDeactivated = 'banned';
end;

function TVkGroup.GetIsDeactivated: Boolean;
begin
  Result := not FDeactivated.IsEmpty;
end;

function TVkGroup.GetIsDeleted: Boolean;
begin
  Result := FDeactivated = 'deleted';
end;

{ TVkGroupStatus }

function TVkGroupStatus.GetStatus: TVkGroupStatusType;
begin
  if FStatus = 'none' then
    Exit(gsNone);
  if FStatus = 'online' then
    Exit(gsOnline);
  if FStatus = 'answer_mark' then
    Exit(gsAnswermark);
  Result := gsNone;
end;

end.

