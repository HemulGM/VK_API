unit VK.Types;

interface

{$INCLUDE include.inc}

{$SCOPEDENUMS ON}

uses
  System.Classes, System.UITypes, REST.Json, System.SysUtils, System.Types, System.Generics.Collections, System.JSON,
  VK.Entity.Common;

type
  TVkException = Exception;

  TVkAuthException = TVkException;

  TVkCaptchaException = TVkException;

  TVkParserException = TVkException;

  TVkHandlerException = TVkException;

  TVkMethodException = class(TVkException)
  private
    FCode: Integer;
    procedure SetCode(const Value: Integer);
  public
    property Code: Integer read FCode write SetCode;
    constructor Create(const Msg: string; Code: Integer);
  end;

  TVkLongPollServerException = TVkException;

  TVkLongPollServerParseException = TVkLongPollServerException;

  TVkLongPollServerHTTPException = TVkLongPollServerException;

  TVkGroupEventsException = TVkLongPollServerException;

  TVkUserEventsException = TVkLongPollServerException;

const
  //Inner VK errors
  ERROR_VK_UNKNOWN = -1;
  ERROR_VK_NOTOKEN = -2;
  ERROR_VK_INTERNAL = -3;
  ERROR_VK_NETWORK = -4;
  ERROR_VK_PARSE = -5;
  ERROR_VK_AUTH = -6;
  ERROR_VK_LONGPOLL = -7;

  //Message Flags
  MF_UNREAD = 1;
  MF_OUTBOX = 2;
  MF_REPLIED = 4;
  MF_IMPORTANT = 8;
  MF_CHAT = 16;
  MF_FRIENDS = 32;
  MF_SPAM = 64;
  MF_DELЕTЕD = 128;
  MF_FIXED = 256;
  MF_MEDIA = 512;
  MF_UNKNOWN_1 = 1024;
  MF_UNKNOWN_2 = 2048;
  MF_UNKNOWN_3 = 4096;
  MF_UNREAD_MULTICHAT = 8192;
  MF_UNKNOWN_4 = 16384;
  MF_UNKNOWN_5 = 32768;
  MF_HIDDEN = 65536;
  MF_DELETE_FOR_ALL = 131072;
  MF_NOT_DELIVERED = 262144;
  MF_UNKNOWN_6 = 524288;
  MF_UNKNOWN_7 = 1048576;
  MF_UNKNOWN_8 = 2097152;
  MF_UNKNOWN_9 = 4194304;

  //Права доступа для токена пользователя
  PERM_NOTIFY = 1;
  PERM_FRIENDS = 2;
  PERM_PHOTOS = 4;
  PERM_AUDIO = 8;
  PERM_VIDEO = 16;
  //PERM_UNKNOWN1 = 32;
  PERM_STORIES = 64;
  PERM_PAGES = 128;
  PERM_LINK = 256;
  //PERM_UNKNOWN2 = 512;
  PERM_STATUS = 1024;
  PERM_NOTES = 2048;
  PERM_MESSAGES = 4096;
  PERM_WALL = 8192;
  //PERM_UNKNOWN3 = 16384;
  PERM_ADS = 32768;
  PERM_OFFLINE = 65536;
  PERM_DOCS = 131072;
  PERM_GROUPS = 262144;
  PERM_NOTIFICATIONS = 524288;
  PERM_STATS = 1048576;
  //PERM_UNKNOWN4 = 2097152;
  PERM_EMAIL = 4194304;
  //PERM_UNKNOWN5 = 8388608;
  //PERM_UNKNOWN6 = 16777216;
  //PERM_UNKNOWN7 = 33554432;
  //PERM_UNKNOWN8 = 67108864;
  PERM_MARKET = 134217728;

  //Права доступа для токена сообщества
  PERM_G_STORIES = 1;
  PERM_G_PHOTOS = 4;
  PERM_G_APP_WIDGET = 64;
  PERM_G_MESSAGES = 4096;
  PERM_G_DOCS = 131072;
  PERM_G_MANAGE = 262144;

  //Audio Genres
  AG_NONE = 0;
  AG_ROCK = 1;
  AG_POP = 2;
  AG_RAPANDHIPHOP = 3;
  AG_EASYLISTENING = 4;
  AG_HOUSEANDDANCE = 5;
  AG_INSTRUMENTAL = 6;
  AG_METAL = 7;
  AG_ALTERNATIVE = 21;
  AG_DUBSTEP = 8;
  AG_JAZZANDBLUES = 1001;
  AG_DRUMANDBASS = 10;
  AG_TRANCE = 11;
  AG_CHANSON = 12;
  AG_ETHNIC = 13;
  AG_ACOUSTICANDVOCAL = 14;
  AG_REGGAE = 15;
  AG_CLASSICAL = 16;
  AG_INDIEPOP = 17;
  AG_SPEECH = 19;
  AG_ELECTROPOPANDDISCO = 22;
  AG_OTHER = 18;

  //Group Dialog Flags
  GR_IMPORTANT = 1;
  GR_UNANSWERED = 2;

  //
  VK_CHAT_ID_START = 2000000000;
  VK_GROUP_ID_START = 1000000000;

type
  {$IFDEF OLD_VERSION}
  TArrayOfString = array of string;

  {$ELSE}

  TArrayOfString = TArray<string>;
  {$ENDIF}

  TArrayOfStringHelper = record helper for TArrayOfString
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    procedure Assign(Source: TStrings); overload;
    function IsEmpty: Boolean;
    function Length: Integer;
  end;

  {$IFDEF OLD_VERSION}
  TArrayOfInteger = array of Integer;
  {$ELSE}

  TArrayOfInteger = TArray<Integer>;
  {$ENDIF}

  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    function Add(Value: Integer): Integer;
    function IsEmpty: Boolean;
    function Length: Integer;
  end;

  TFields = TArrayOfString;

  TParam = TArrayOfString;

  TParamInt = TArrayOfInteger;

  TIdList = TArrayOfInteger;

  {$IFDEF OLD_VERSION}
  TParams = array of TParam;

  TParamsInt = array of TParamInt;

  {$ELSE}

  TParams = TArray<TParam>;

  TParamsInt = TArray<TParamInt>;
  {$ENDIF}

  TParamsHelper = record helper for TParams
  private
    function AddParam(var Dest: TParams; Param: TParam): Integer; inline;
  public
    function Add(Param: TParam): Integer; overload; inline;
    function Add(Key: string; Value: string): Integer; overload; inline;
    function Add(Key: string; Value: Integer): Integer; overload; inline;
    function Add(Key: string; Value: Extended): Integer; overload; inline;
    function Add(Key: string; Value: TDateTime): Integer; overload; inline;
    function Add(Key: string; Value: Boolean): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfString): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfInteger): Integer; overload; inline;
    function KeyExists(Key: string): Boolean; inline;
    function GetValue(Key: string): string; inline;
    function Remove(Key: string): string; inline;
  end;

type
  TWalkMethod = reference to function(Offset: Integer; var Cancel: Boolean): Integer;

  TVkValidationType = (Unknown, SMS, App);

  TVkValidationTypeHelper = record helper for TVkValidationType
    class function FromString(const Value: string): TVkValidationType; static;
  end;

  TOn2FA = reference to function(const ValidationType: TVkValidationType; var Code: string; var Remember: Boolean): Boolean;

  TAttachment = record
    OwnerId, Id: Integer;
    AccessKey: string;
    &Type: string;
  public
    class function Photo(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Video(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Audio(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Doc(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Link(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Market(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function MarketAlbum(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Wall(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function WallReply(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Sticker(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Gift(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Album(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Create(&Type: string; OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class operator Implicit(const Value: string): TAttachment;
    class operator Implicit(const Value: TAttachment): string;
    function ToString: string; inline;
  end;

  TAttachmentArray = TArray<TAttachment>;

  TAttachmentArrayHelper = record helper for TAttachmentArray
    function ToStrings: TArray<string>; inline;
    function IsEmpty: Boolean; inline;
  end;

  TVkPhotoFeedType = (Photo, PhotoTag);

  TVkPhotoFeedTypeHelper = record helper for TVkPhotoFeedType
    function ToString: string; inline;
  end;

  TVkPhotoSystemAlbum = (Wall, Saved, Profile);

  TVkPhotoSystemAlbumHelper = record helper for TVkPhotoSystemAlbum
    function ToString: string; inline;
    function ToVkId: Integer; inline;
  end;

  /// <summary>
  ///  Флаги сообщений
  /// </summary>
  TVkMessageFlag = (UNKNOWN_9, UNKNOWN_8, UNKNOWN_7, UNKNOWN_6, NotDelivered, //
    DeleteForAll, Hidden, UNKNOWN_5, UNKNOWN_4, UnreadMultichat, UNKNOWN_3,   //
    UNKNOWN_2, UNKNOWN_1, Media, Fixed, Deleted, Spam, Friends, Chat,         //
    Important, Replied, Outbox, Unread);

  TVkMessageFlagHelper = record helper for TVkMessageFlag
    function ToString: string; inline;
  end;

  TVkMessageFlags = set of TVkMessageFlag;

  TMessageFlagsHelper = record helper for TVkMessageFlags
    class function FlagDataToFlag(FlagData: Integer): TVkMessageFlag; static;
    class function Create(Data: Integer): TVkMessageFlags; static;
    function ToString: string;
  end;

  /// <summary>
  /// ChatPhotoUpdate — обновлена фотография беседы;
  /// ChatPhotoRemove — удалена фотография беседы;
  /// ChatCreate — создана беседа;
  /// ChatTitleUpdate — обновлено название беседы;
  /// ChatInviteUser — приглашен пользователь;
  /// ChatKickUser — исключен пользователь;
  /// ChatPinMessage — закреплено сообщение;
  /// ChatUnpinMessage — откреплено сообщение;
  /// ChatInviteUserByLink — пользователь присоединился к беседе по ссылке.
  /// </summary>
  TVkMessageActionType = (Unknown, ChatPhotoUpdate, ChatPhotoRemove,          //
    ChatCreate, ChatTitleUpdate, ChatInviteUser, ChatKickUser,                //
    ChatPinMessage, ChatUnpinMessage, ChatInviteUserByLink);

  TVkMessageActionTypeHelper = record helper for TVkMessageActionType
    function ToString: string; inline;
    class function Create(const Value: string): TVkMessageActionType; static;
  end;

  /// <summary>
  ///  Жанры музыки
  /// </summary>
  TVkAudioGenre = (None, Rock, Pop, RapAndHipHop, EasyListening,    //
    HouseAndDance, Instrumental, Metal, Alternative, Dubstep,       //
    JazzAndBlues, DrumAndBass, Trance, Chanson, Ethnic,             //
    AcousticAndVocal, Reggae, Classical, IndiePop, Speech,          //
    ElectropopAndDisco, Other);

  TVkAudioGenreHelper = record helper for TVkAudioGenre
    function ToConst: Integer;
    function ToString: string; inline;
    class function Create(Value: Integer): TVkAudioGenre; static;
  end;

  TVkSort = (Asc, Desc);

  TVkSortHelper = record helper for TVkSort
    function ToString: string; overload; inline;
  end;

  TVkSortIdTime = (IdAsc, IdDesc, TimeAsc, TimeDesc);

  TVkSortIdTimeHelper = record helper for TVkSortIdTime
    function ToString: string; overload; inline;
  end;

  TVkLang = (Auto = -1, RU = 0, UK = 1, BE = 2, EN = 3, ES = 4,   //
    FI = 5, DE = 6, IT = 7);

  TVkGroupAdminLevel = (None, Moderator, Editor, Administrator);

  TVkGroupStatusType = (None, Online, AnswerMark);

  TVkGroupStatusTypeHelper = record helper for TVkGroupStatusType
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupStatusType; static;
  end;

  /// <summary>
  ///  <b>Friends</b> — будут возвращены только друзья в этом сообществе.
  ///  <b>Unsure</b> — будут возвращены пользователи, которые выбрали «Возможно
  ///  пойду» (если сообщество относится к мероприятиям).
  ///  <b>Managers</b> — будут возвращены только руководители сообщества
  ///  (доступно при запросе с передачей access_token от имени администратора сообщества).
  /// </summary>
  TVkGroupMembersFilter = (Friends, Unsure, Managers);

  TVkGroupMembersFilterHelper = record helper for TVkGroupMembersFilter
    function ToString: string; inline;
  end;

  /// <summary>
  /// (автовыбор), Запустить, Играть, Перейти, Открыть, Подробнее, Позвонить,
  /// Забронировать, Записаться, Зарегистрироваться, Купить, Купить билет,
  /// Заказать, Создать, Установить, Связаться, Заполнить, Подписаться,
  /// Я пойду, Вступить, Связаться, Написать, Начать, Получить, Смотреть,
  /// Скачать, Участвовать, Играть, Подать заявку, Получить предложение,
  /// Написать, Откликнуться
  /// Подробное описание тут https://vk.com/dev/wall.postAdsStealth
  /// </summary>
  TVkPostLinkButton = (Auto, AppJoin, AppGameJoin, OpenUrl, Open, More, Call, //
    Book, Enroll, Register, Buy, BuyTicket, Order, Create, Install, Contact,  //
    Fill, JoinPublic, JoinEvent, Join, IM, IM2, Start, Get, Watch, Download,  //
    Participate, Play, Apply, GetAnOffer, ToWrite, Reply);

  TVkPostLinkButtonHelper = record helper for TVkPostLinkButton
    function ToString: string; inline;
  end;

  TVkProfileField = (PhotoId, Verified, Sex, BirthDate, City, Country,           //
    HomeTown, HasPhoto, Photo50, Photo100, Photo200Orig, Photo200, Photo400Orig, //
    PhotoMax, PhotoMaxOrig, PhotoBig, PhotoMedium, Online, Lists, Domain,        //
    HasMobile, Contacts, Site, Education, Universities, Schools, Status,         //
    LastSeen, FollowersCount, CommonCount, Occupation, Nickname, Relatives,      //
    Relation, Personal, Connections, &Exports, WallComments, Activities,         //
    Interests, Music, Movies, TV, Books, Games, About, Quotes, CanPost,          //
    CanSeeAllPosts, CanSeeAudio, CanWritePrivateMessage, CanSendFriendRequest,   //
    IsFavorite, IsHiddenFromFeed, TimeZone, ScreenName, MaidenName, CropPhoto,   //
    IsFriend, FriendStatus, Career, Military, Blacklisted, BlacklistedByMe,      //
    CanBeInvitedGroup);

  TVkProfileFieldHelper = record helper for TVkProfileField
    function ToString: string; inline;
  end;

  TVkProfileFields = set of TVkProfileField;

  TVkProfileFieldsHelper = record helper for TVkProfileFields
  public
    function ToString: string; inline;
    class function All: TVkProfileFields; static; inline;
  end;

  TVkGroupField = (City, Country, Place, Description, WikiPage, MembersCount, //
    Counters, StartDate, FinishDate, CanPost, CanSeeAllPosts, Activity,       //
    Status, Contacts, Links, FixedPost, Verified, Site, CanCreateTopic,       //
    Photo50);

  TVkGroupFieldHelper = record helper for TVkGroupField
    function ToString: string; inline;
  end;

  TVkGroupFields = set of TVkGroupField;

  TVkGroupFieldsHelper = record helper for TVkGroupFields
    function ToString: string; inline;
    class function All: TVkGroupFields; static; inline;
  end;

  TVkGroupAddressField = (Title, Address, AdditionalAddress, CountryId,       //
    CityId, MetroStationId, Latitude, Longitude, WorkInfoStatus, TimeOffset);

  TVkGroupAddressFieldHelper = record helper for TVkGroupAddressField
    function ToString: string; inline;
  end;

  TVkGroupAddressFields = set of TVkGroupAddressField;

  TVkGroupAddressFieldsHelper = record helper for TVkGroupAddressFields
    function ToString: string; inline;
    class function All: TVkGroupAddressFields; static; inline;
  end;

  TVkGroupAccess = (Open, Close, &Private);

  TVkGroupRole = (Moderator, Editor, Admin);

  TVkGroupRoleHelper = record helper for TVkGroupRole
    function ToString: string; inline;
  end;

  TVkGroupMainSection = (None, Photos, Board, Audios, Videos, Market);

  /// <summary>
  /// GroupTagColor
  /// Тут список цветов -> VkGroupTagColors
  /// </summary>
  TVkGroupTagColor = string;

  TVkDeactivated = (None, Deleted, Banned);

  TVkDeactivatedHelper = record helper for TVkDeactivated
    function ToString: string; inline;
    class function Create(const Value: string): TVkDeactivated; static;
  end;

  TVkAgeLimits = (Unknown = 0, None = 1, Plus16 = 2, Plus18 = 3);

  TVkCurrency = (RUB, UAH, KZT, EUR, USD);

  TVkMarketCurrencyHelper = record helper for TVkCurrency
    function ToConst: Integer; inline;
  end;

  TVkGroupTypeCreate = (Group, Event, &Public);

  TVkGroupTypeCreateHelper = record helper for TVkGroupTypeCreate
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupTypeCreate; static;
  end;

  TVkGroupType = (Group, Event, Page);

  TVkGroupTypeHelper = record helper for TVkGroupType
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupType; static;
  end;

  TVkGroupFilter = (Admin, Editor, Moder, Advertiser, Groups, Publics,        //
    Events, HasAddress);

  TVkGroupFilterHelper = record helper for TVkGroupFilter
    function ToString: string; inline;
  end;

  TVkGroupFilters = set of TVkGroupFilter;

  TVkGroupFiltersHelper = record helper for TVkGroupFilters
    function ToString: string; inline;
    class function All: TVkGroupFilters; static; inline;
  end;

  /// <summary>
  ///  Флаги диалогов
  /// </summary>
  TVkDialogFlag = (Important, Unanswered);

  TVkDialogFlags = set of TVkDialogFlag;

  TVkDialogFlagsHelper = record helper for TVkDialogFlags
    function ToString: string; overload; inline;
    class function FlagDataToFlag(FlagData: Integer): TVkDialogFlag; static;
    class function Create(Data: Integer): TVkDialogFlags; static;
  end;

  /// <summary>
  ///  Идентификатор типа изменения в чате
  /// </summary>

  TVkChatChangeInfoType = (None, Name, Pic, NewAdmin, FixMessage, Join,       //
    Leave, Kick, Unadmin);

  /// <summary>
  ///  Платформы
  /// </summary>
  TVkPlatform = (Unknown, Mobile, IPhone, IPad, Android, WindowsPhone,        //
    Windows, Web);

  TVkPlatformHelper = record helper for TVkPlatform
    function ToString: string; inline;
    class function Create(const Value: string): TVkPlatform; static;
  end;

  /// <summary>
  /// Cтатус заявки
  /// Processing – заявка рассматривается;
  /// Declined – заявка отклонена;
  /// Response – общий ответ по статусу обработки заявки;
  /// ResponseWithLink – общий ответ по статусу обработки заявки, содержащий ссылку в поле link;
  /// </summary>
  TVkNameRequestStatus = (Processing, Declined, Response, ResponseWithLink);

  TVkNameRequestStatusHelper = record helper for TVkNameRequestStatus
    class function Create(const Value: string): TVkNameRequestStatus; static;
    function ToString: string; inline;
  end;

  /// <summary>
  ///  Тип смены флагов
  /// </summary>
  TVkFlagsChangeType = (Replace, &Set, Reset);

  TVkFlagsChangeTypeHelper = record helper for TVkFlagsChangeType
    function ToString: string; overload; inline;
  end;

  /// <summary>
  ///  Типы объектов
  /// </summary>
  TVkItemType = (Post, Comment, Photo, Audio, Video, Note, Market,            //
    PhotoComment, VideoComment, TopicComment, MarketComment, Sitepage, Story);

  TVkItemTypeHelper = record helper for TVkItemType
    function ToString: string; inline;
  end;

  /// <summary>
  ///  Типы вложений
  /// </summary>
  TVkAttachmentType = (Unknown, Photo, Video, Audio, Doc, Link, Market,       //
    MarketAlbum, Wall, WallReply, Sticker, Gift, Call, AudioMessage,          //
    PostedPhoto, Graffiti, Note, App, Poll, Page, Album, PhotosList,          //
    PrettyCards, Event);

  TVkAttachmentTypeHelper = record helper for TVkAttachmentType
    function ToString: string; inline;
    class function Create(Value: string): TVkAttachmentType; static;
  end;

  TVkDocumentType = (None, Text, Archive, GIF, Picture, Audio, Video,         //
    Book, Unknown);

  TVkPeerType = (Unknown, User, Chat, Group, Email);

  TVkPeerTypeHelper = record helper for TVkPeerType
    function ToString: string; inline;
    class function Create(Value: string): TVkPeerType; static;
  end;

  TVkKeyboardButtonColor = (Positive, Negative, Primary, Secondary);

  TVkKeyboardButtonColorHelper = record helper for TVkKeyboardButtonColor
    function ToString: string; inline;
    function ToColor: TAlphaColor; inline;
    class function Create(Value: string): TVkKeyboardButtonColor; static;
  end;

  TVkPostType = (Suggests, Postponed, Owner, Others, All);

  TVkPostTypeHelper = record helper for TVkPostType
    function ToString: string; inline;
  end;

  TVkPolitical = (None, Communist, Socialist, Moderate, Liberal,              //
    Conservative, Monarchical, UltraConservative, Indifferent, Libertarian);

  /// <summary>
  /// именительный – nom, родительный – gen, дательный – dat, винительный – acc,
  /// творительный – ins, предложный – abl
  /// </summary>
  TVkNameCase = (Nom, Gen, Dat, Acc, Ins, Abl);

  TVkNameCaseHelper = record helper for TVkNameCase
    function ToString: string; inline;
  end;

  /// <summary>
  ///  Пол
  /// </summary>
  TVkSex = (None, Female, Male);

  /// <summary>
  ///  Пол - поиск
  /// </summary>
  TVkSexSearch = (Any, Male, Female);

  /// <summary>
  ///  Видимость даты рождения
  /// </summary>
  TVkBirthDateVisibility = (Hidden, Visible, DayMonOnly);

  /// <summary>
  /// Отношения.
  /// None — не указано.
  /// NotMarried — не женат/не замужем;
  /// HaveFriend — есть друг/есть подруга;
  /// Affiance — помолвлен/помолвлена;
  /// Married — женат/замужем;
  /// Complicated — всё сложно;
  /// ActivelyLooking — в активном поиске;
  /// InLove — влюблён/влюблена;
  /// CivilMarriage — в гражданском браке;
  /// </summary>
  TVkRelation = (None, NotMarried, HaveFriend, Affiance, Married, Complicated, //
    ActivelyLooking, InLove, CivilMarriage);

  TVkLinkStatusType = (lsNotBanned, lsBanned, lsProcessing);

  TVkLinkStatusTypeHelper = record helper for TVkLinkStatusType
    function ToString: string; overload; inline;
    class function FromString(Value: string): TVkLinkStatusType; static; inline;
  end;

  TVkUserReport = (Porn, Spam, Insult, Advertisеment);

  TVkUserReportHelper = record helper for TVkUserReport
    function ToString: string; overload; inline;
  end;

  TVkUserBlockReason = (Other, Spam, InsultingParticipants,                   //
    ObsceneExpressions, OffTopic);

  TUserBlockReasonHelper = record helper for TVkUserBlockReason
    function ToString: string; overload; inline;
    function ToConst: Integer; overload; inline;
  end;

  TVkMediaReportReason = (Spam, ChildPorn, Extremism, Violence, Drug, Adults, //
    Insult, CallForSuicide);

  TVkMediaReportReasonHelper = record helper for TVkMediaReportReason
    function ToString: string; overload; inline;
    function ToConst: Integer; overload; inline;
  end;

  /// <summary>
  /// join — пользователь вступил в группу или мероприятие (подписался на публичную страницу).
  /// unsure — для мероприятий: пользователь выбрал вариант «Возможно, пойду».
  /// accepted — пользователь принял приглашение в группу или на мероприятие.
  /// approved — заявка на вступление в группу/мероприятие была одобрена руководителем сообщества.
  /// request — пользователь подал заявку на вступление в сообщество.
  /// </summary>
  TVkGroupJoinType = (Unknown, Join, Unsure, Accepted, Approved, Request);

  TVkGroupJoinTypeHelper = record helper for TVkGroupJoinType
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkGroupJoinType; static;
  end;

  TVkGroupLevel = (None, Moder, Editor, Admin);

  TVkGroupLevelHelper = record helper for TVkGroupLevel
    function ToString: string; overload; inline;
  end;

  TVkCounterFilter = (Friends, Messages, Photos, Videos, Notes, Gifts,        //
    Events, Groups, Notifications, Sdk, AppRequests, FriendsRecommendations);

  TVkCounterFilterHelper = record helper for TVkCounterFilter
    function ToString: string; overload; inline;
  end;

  TVkCounterFilters = set of TVkCounterFilter;

  TVkCounterFiltersHelper = record helper for TVkCounterFilters
    function ToString: string; overload; inline;
  end;

  /// <summary>
  /// Статус заказа
  /// New - новый;
  /// Approved - согласуется;
  /// Assembled - собирается;
  /// Delivered - доставляется;
  /// Completed - выполнен;
  /// Canceled - отменен;
  /// Returned - возвращен.
  /// </summary>
  TVkOrderStatus = (New, Approved, Assembled, Delivered, Completed, Canceled, //
    Returned);

  /// <summary>
  /// Статус доступности товара
  /// 0 — товар доступен;
  /// 1 — товар удален;
  /// 2 — товар недоступен.
  /// </summary>
  TVkProductAvailability = (Available, Removed, NotAvailable);

  TVkInfoFilter = (Country, HttpsRequired, OwnPostsDefault, NoWallReplies,    //
    Intro, Lang);

  TVkInfoFilterHelper = record helper for TVkInfoFilter
    function ToString: string; overload; inline;
  end;

  TVkInfoFilters = set of TVkInfoFilter;

  TVkInfoFiltersHelper = record helper for TVkInfoFilters
    function ToString: string; overload; inline;
  end;

  TVkPermission = (Notify, Friends, Photos, Audio, Video, Stories, Pages,     //
    Status, Notes, Messages, Wall, Ads, Offline, Docs, Groups, Notifications, //
    Stats, Email, Market, AppWidget, Manage);

  TVkPermissionHelper = record helper for TVkPermission
    function ToString: string; overload; inline;
  end;

  TVkPermissions = set of TVkPermission;

  TVkPermissionsHelper = record helper for TVkPermissions
    function ToString: string; overload; inline;
    procedure Include(Value: TVkPermission); inline;
  end;

  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(Sender: TObject; const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; E: Exception; Code: Integer; Text: string) of object;

const
  //Вспомогательное текстовое представление
  VkUserActive: array[Boolean] of string = ('Бездействие', 'Покинул сайт');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone', 'iPad', 'Android',
    'Windows Phone', 'Windows', 'Web');
  VkGroupLevel: array[TVkGroupLevel] of string = ('Участник', 'Модератор', 'Редактор', 'Администратор');
  VkUserBlockReason: array[TVkUserBlockReason] of string = ('Другое', 'Спам', 'Оскорбление участников',
    'Мат', 'Разговоры не по теме');
  VkPhotoReportReason: array[TVkMediaReportReason] of string = ('Спам', 'Детская порнография', 'Экстремизм',
    'Насилие', 'Пропаганда наркотиков', 'Материал для взрослых', 'Оскорбление', 'Призыв к суициду');
  VkMessageFlagTypes: array[TVkMessageFlag] of string = ('Unknown_9', 'Unknown_8', 'Unknown_7', 'Unknown_6',
    'NotDelivered', 'DeleteForAll', 'Hidden', 'Unknown_5', 'Unknown_4', 'UnreadMultichat', 'Unknown_3',
    'Unknown_2', 'Unknown_1', 'Media', 'Fixed', 'Deleted', 'Spam', 'Friends', 'Chat', 'Important',
    'Replied', 'Outbox', 'Unread');
  VkAudioGenresStr: array[TVkAudioGenre] of string = ('', 'Rock', 'Pop', 'RapAndHipHop', 'EasyListening',
    'HouseAndDance', 'Instrumental', 'Metal', 'Alternative', 'Dubstep', 'JazzAndBlues', 'DrumAndBass',
    'Trance', 'Chanson', 'Ethnic', 'AcousticAndVocal', 'Reggae', 'Classical', 'IndiePop', 'Speech',
    'ElectropopAndDisco', 'Other');
  VkChatChangeInfoType: array[TVkChatChangeInfoType] of string = ('', 'Изменилось название беседы',
    'Сменилась обложка беседы', 'Назначен новый администратор', 'Закреплено сообщение',
    'Пользователь присоединился к беседе', 'Пользователь покинул беседу', 'Пользователя исключили из беседы',
    'С пользователя сняты права администратора');

const
  //Аналогии типов
  //Соответствия флагов сообщений и битов
  VkMessageFlags: array[TVkMessageFlag] of Integer = (MF_UNKNOWN_9, MF_UNKNOWN_8,
    MF_UNKNOWN_7, MF_UNKNOWN_6, MF_NOT_DELIVERED, MF_DELETE_FOR_ALL, MF_HIDDEN,
    MF_UNKNOWN_5, MF_UNKNOWN_4, MF_UNREAD_MULTICHAT, MF_UNKNOWN_3, MF_UNKNOWN_2,
    MF_UNKNOWN_1, MF_MEDIA, MF_FIXED, MF_DELЕTЕD, MF_SPAM, MF_FRIENDS, MF_CHAT,
    MF_IMPORTANT, MF_REPLIED, MF_OUTBOX, MF_UNREAD);
  VkAudioGenres: array[TVkAudioGenre] of Integer = (AG_NONE, AG_ROCK, AG_POP,
    AG_RAPANDHIPHOP, AG_EASYLISTENING, AG_HOUSEANDDANCE, AG_INSTRUMENTAL,
    AG_METAL, AG_ALTERNATIVE, AG_DUBSTEP, AG_JAZZANDBLUES, AG_DRUMANDBASS,
    AG_TRANCE, AG_CHANSON, AG_ETHNIC, AG_ACOUSTICANDVOCAL, AG_REGGAE,
    AG_CLASSICAL, AG_INDIEPOP, AG_SPEECH, AG_ELECTROPOPANDDISCO, AG_OTHER);
  VkDialogFlags: array[TVkDialogFlag] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  //
  VkPlatformsType: array[TVkPlatform] of string = ('', 'mobile', 'ios', 'ios', 'android', 'winphone', 'windows', 'web');
  VkAttachmentType: array[TVkAttachmentType] of string = ('', 'photo', 'video', 'audio', 'doc', 'link', 'market',
    'market_album', 'wall', 'wall_reply', 'sticker', 'gift', 'call', 'audio_message', 'posted_photo', 'graffiti',
    'note', 'app', 'poll', 'page', 'album', 'photos_list', 'pretty_cards', 'event');
  VkPeerType: array[TVkPeerType] of string = ('', 'user', 'chat', 'group', 'email');
  VkNameCase: array[TVkNameCase] of string = ('nom', 'gen', 'dat', 'acc', 'ins', 'abl');
  VkItemType: array[TVkItemType] of string = ('post', 'comment', 'photo', 'audio', 'video', 'note', 'market',
    'photo_comment', 'video_comment', 'topic_comment', 'market_comment', 'sitepage', 'story');
  VkGroupJoinType: array[TVkGroupJoinType] of string = ('', 'join', 'unsure', 'accepted', 'approved', 'request');
  VkPostType: array[TVkPostType] of string = ('suggests', 'postponed', 'owner', 'others', 'all');
  VkPremissionStr: array[TVkPermission] of string = ('notify', 'friends', 'photos', 'audio', 'video', 'stories',
    'pages', 'status', 'notes', 'messages', 'wall', 'ads', 'offline', 'docs', 'groups', 'notifications', 'stats',
    'email', 'market', 'app_widget', 'manage');
  VkProfileField: array[TVkProfileField] of string = (
    'photo_id', 'verified', 'sex', 'bdate', 'city', 'country', 'home_town', 'has_photo', 'photo_50',
    'photo_100', 'photo_200_orig', 'photo_200', 'photo_400_orig', 'photo_max', 'photo_max_orig', 'photo_big',
    'photo_medium', 'online', 'lists', 'domain', 'has_mobile', 'contacts', 'site', 'education', 'universities',
    'schools', 'status', 'last_seen', 'followers_count', 'common_count', 'occupation', 'nickname',
    'relatives', 'relation', 'personal', 'connections', 'exports', 'wall_comments', 'activities', 'interests',
    'music', 'movies', 'tv', 'books', 'games', 'about', 'quotes', 'can_post', 'can_see_all_posts',
    'can_see_audio', 'can_write_private_message', 'can_send_friend_request', 'is_favorite',
    'is_hidden_from_feed', 'timezone', 'screen_name', 'maiden_name', 'crop_photo', 'is_friend',
    'friend_status', 'career', 'military', 'blacklisted', 'blacklisted_by_me', 'can_be_invited_group');
  VKPostLinkButton: array[TVkPostLinkButton] of string = (
    'auto', 'app_join', 'app_game_join', 'open_url', 'open', 'more', 'call', 'book', 'enroll', 'register', 'buy',
    'buy_ticket', 'order', 'create', 'install', 'contact', 'fill', 'join_public', 'join_event', 'join', 'im',
    'im2', 'begin', 'get', 'watch', 'download', 'participate', 'play', 'apply', 'get_an_offer', 'to_write', 'reply');
  VkGroupField: array[TVkGroupField] of string = ('city', 'country', 'place', 'description', 'wiki_page', 'members_count',
    'counters', 'start_date', 'finish_date', 'can_post', 'can_see_all_posts', 'activity', 'status',
    'contacts', 'links', 'fixed_post', 'verified', 'site', 'can_create_topic', 'photo_50');
  VkGroupFilter: array[TVkGroupFilter] of string = ('admin', 'editor', 'moder', 'advertiser', 'groups', 'publics',
    'events', 'hasAddress');
  VkGroupRole: array[TVkGroupRole] of string = ('moderator', 'editor', 'administrator');
  VkGroupMembersFilter: array[TVkGroupMembersFilter] of string = ('friends', 'unsure', 'managers');
  VkCounterFilter: array[TVkCounterFilter] of string = ('friends', 'messages', 'photos', 'videos', 'notes',
    'gifts', 'events', 'groups', 'notifications', 'sdk', 'app_requests', 'friends_recommendations');
  VkInfoFilter: array[TVkInfoFilter] of string = ('country', 'https_required', 'own_posts_default', 'no_wall_replies',
    'intro', 'lang');
  VkCurrencyId: array[TVkCurrency] of Integer = (643, 980, 398, 978, 840);
  VkGroupAddressField: array[TVkGroupAddressField] of string = ('title', 'address', 'additional_address', 'country_id',
    'city_id', 'metro_station_id', 'latitude', 'longitude', 'work_info_status', 'time_offset');
  VkGroupTagColors: array of TVkGroupTagColor = ['4bb34b', '5c9ce6', 'e64646', '792ec0', '63b9ba', 'ffa000',
    'ffc107', '76787a', '9e8d6b', '45678f', '539b9c', '454647', '7a6c4f', '6bc76b', '5181b8', 'ff5c5c',
    'a162de', '7ececf', 'aaaeb3', 'bbaa84'];
  VkDeactivated: array[TVkDeactivated] of string = ('', 'deleted', 'banned');
  VkNameRequestStatus: array[TVkNameRequestStatus] of string = ('processing', 'declined', 'response', 'response_with_link');
  VkMessageActionType: array[TVkMessageActionType] of string = ('', 'chat_photo_update', 'chat_photo_remove',
    'chat_create', 'chat_title_update', 'chat_invite_user', 'chat_kick_user', 'chat_pin_message', 'chat_unpin_message',
    'chat_invite_user_by_link');
  VkKeyboardButtonColor: array[TVkKeyboardButtonColor] of string = ('positive', 'negative', 'primary', 'secondary');
  VkKeyboardButtonColorValue: array[TVkKeyboardButtonColor] of TAlphaColor = ($FF4BB34B, $FFE64646, $FF5181B8, $FFFFFFFF);
  VkGroupStatusType: array[TVkGroupStatusType] of string = ('none', 'online', 'answer_mark');
  VkGroupTypeCreate: array[TVkGroupTypeCreate] of string = ('group', 'event', 'public');
  VkGroupType: array[TVkGroupType] of string = ('group', 'event', 'page');
  VkFlagsChangeType: array[TVkFlagsChangeType] of string = ('Replace', 'Set', 'Reset');
  VkPhotoSystemAlbum: array[TVkPhotoSystemAlbum] of string = ('wall', 'saved', 'profile');
  VkPhotoSystemAlbumId: array[TVkPhotoSystemAlbum] of Integer = (-7, -15, -6);
  VkPhotoFeedType: array[TVkPhotoFeedType] of string = ('photo', 'photo_tag');
  VkSortIdTime: array[TVkSortIdTime] of string = ('id_asc', 'id_desc', 'time_asc', 'time_desc');
  VkSort: array[TVkSort] of string = ('asc', 'desc');
  VkUserReport: array[TVkUserReport] of string = ('porn', 'spam', 'insult', 'advertisеment');
  VkLinkStatusType: array[TVkLinkStatusType] of string = ('not_banned', 'banned', 'processing');
  VkValidationType: array[TVkValidationType] of string = ('', '2fa_sms', '2fa_app');

function NormalizePeerId(Value: Integer): Integer;

function PeerIdIsChat(Value: Integer): Boolean;

function PeerIdIsUser(Value: Integer): Boolean;

function PeerIdIsGroup(Value: Integer): Boolean;

implementation

uses
  VK.CommonUtils, System.DateUtils, System.Character, System.StrUtils;

function PeerIdIsChat(Value: Integer): Boolean;
begin
  Result := Value > VK_CHAT_ID_START;
end;

function PeerIdIsUser(Value: Integer): Boolean;
begin
  Result := (Value < VK_GROUP_ID_START) and (Value > 0);
end;

function PeerIdIsGroup(Value: Integer): Boolean;
begin
  Result := (Value > VK_GROUP_ID_START) and (Value < VK_CHAT_ID_START);
end;

function NormalizePeerId(Value: Integer): Integer;
begin
  if Value > VK_CHAT_ID_START then
    Exit(Value - VK_CHAT_ID_START);
  if Value > VK_GROUP_ID_START then
    Exit(-(Value - VK_GROUP_ID_START));
  Result := Value;
end;

{ TMessageChangeTypeHelper }

function TVkFlagsChangeTypeHelper.ToString: string;
begin
  Result := VkFlagsChangeType[Self];
end;

{ TVkPhotoSystemAlbumHelper }

function TVkPhotoSystemAlbumHelper.ToString: string;
begin
  Result := VkPhotoSystemAlbum[Self];
end;

function TVkPhotoSystemAlbumHelper.ToVkId: Integer;
begin
  Result := VkPhotoSystemAlbumId[Self];
end;

{ TVkPhotoFeedTypeHelper }

function TVkPhotoFeedTypeHelper.ToString: string;
begin
  Result := VkPhotoFeedType[Self];
end;

{ TMessageFlagsHelper }

class function TMessageFlagsHelper.Create(Data: Integer): TVkMessageFlags;
var
  i: TVkMessageFlag;
begin
  Result := [];
  for i := Low(VkMessageFlags) to High(VkMessageFlags) do
  begin
    if (Data - VkMessageFlags[i]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkMessageFlags[i]));
      Data := Data - VkMessageFlags[i];
    end;
  end;
end;

class function TMessageFlagsHelper.FlagDataToFlag(FlagData: Integer): TVkMessageFlag;
var
  i: TVkMessageFlag;
begin
  Result := TVkMessageFlag.Chat;
  for i := Low(VkMessageFlags) to High(VkMessageFlags) do
    if VkMessageFlags[i] = FlagData then
      Exit(i);
end;

function TMessageFlagsHelper.ToString: string;
var
  Item: TVkMessageFlag;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TDialogFlagsHelper }

class function TVkDialogFlagsHelper.Create(Data: Integer): TVkDialogFlags;
var
  i: Integer;
begin
  Result := [];
  for i := Ord(TVkDialogFlag.Important) to Ord(TVkDialogFlag.Unanswered) do
  begin
    if (Data - VkDialogFlags[TVkDialogFlag(i)]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkDialogFlags[TVkDialogFlag(i)]));
      Data := Data - VkDialogFlags[TVkDialogFlag(i)];
    end;
  end;
end;

class function TVkDialogFlagsHelper.FlagDataToFlag(FlagData: Integer): TVkDialogFlag;
begin
  Result := TVkDialogFlag(IndexInt(FlagData, VkDialogFlags));
end;

function TVkDialogFlagsHelper.ToString: string;
var
  Flag: TVkDialogFlag;
begin
  for Flag in Self do
    case Flag of
      TVkDialogFlag.Important:
        Result := Result + 'Important ';
      TVkDialogFlag.Unanswered:
        Result := Result + 'Unanswered ';
    end;
end;

{ TArrayOfIntegerHelper }

function TArrayOfIntegerHelper.Add(Value: Integer): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

function TArrayOfIntegerHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfIntegerHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfIntegerHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i].ToString + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfIntegerHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i].ToString;
  end;
end;

{ TArrayOfStringHelper }

function TArrayOfStringHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfStringHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfStringHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i] + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfStringHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i];
  end;
end;

procedure TArrayOfStringHelper.Assign(Source: TStrings);
var
  i: Integer;
begin
  SetLength(Self, Source.Count);
  for i := 0 to Source.Count - 1 do
    Self[i] := Source[i];
end;

{ TParamsHelper }

function TParamsHelper.AddParam(var Dest: TParams; Param: TParam): Integer;
var
  i: Integer;
begin
  for i := Low(Dest) to High(Dest) do
    if Dest[i][0] = Param[0] then
    begin
      Dest[i] := Param;
      Exit(i);
    end;
  Result := Length(Dest) + 1;
  SetLength(Dest, Result);
  Dest[Result - 1] := Param;
end;

function TParamsHelper.Add(Param: TParam): Integer;
begin
  Result := AddParam(Self, Param);
end;

function TParamsHelper.Add(Key, Value: string): Integer;
begin
  Result := AddParam(Self, [Key, Value]);
end;

function TParamsHelper.Add(Key: string; Value: Integer): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Boolean): Integer;
begin
  Result := AddParam(Self, [Key, BoolToString(Value)]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfInteger): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TDateTime): Integer;
begin
  Result := AddParam(Self, [Key, DateTimeToUnix(Value, False).ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Extended): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.GetValue(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(Self[i][1]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfString): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.KeyExists(Key: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(True);
end;

function TParamsHelper.Remove(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
    begin
      Delete(Self, i, 1);
      Break;
    end;
end;

{ TVkAudioGenreHelper }

class function TVkAudioGenreHelper.Create(Value: Integer): TVkAudioGenre;
begin
  Result := TVkAudioGenre(IndexInt(Value, VkAudioGenres));
end;

function TVkAudioGenreHelper.ToConst: Integer;
begin
  Result := VkAudioGenres[Self];
end;

function TVkAudioGenreHelper.ToString: string;
begin
  Result := VkAudioGenresStr[Self];
end;

{ TGroupJoinTypeHelper }

class function TVkGroupJoinTypeHelper.Create(Value: string): TVkGroupJoinType;
begin
  Result := TVkGroupJoinType(IndexStr(Value, VkGroupJoinType));
end;

function TVkGroupJoinTypeHelper.ToString: string;
begin
  Result := VkGroupJoinType[Self];
end;

{ TUserBlockReasonHelper }

function TUserBlockReasonHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

function TUserBlockReasonHelper.ToString: string;
begin
  Result := VkUserBlockReason[Self];
end;

{ TVkGroupLevelHelper }

function TVkGroupLevelHelper.ToString: string;
begin
  Result := VkGroupLevel[Self];
end;

{ TVkItemTypeHelper }

function TVkItemTypeHelper.ToString: string;
begin
  Result := VkItemType[Self];
end;

{ TVkNameCaseHelper }

function TVkNameCaseHelper.ToString: string;
begin
  Result := VkNameCase[Self];
end;

{ TVkAttachmentTypeHelper }

class function TVkAttachmentTypeHelper.Create(Value: string): TVkAttachmentType;
begin
  Result := TVkAttachmentType(IndexStr(Value, VkAttachmentType));
end;

function TVkAttachmentTypeHelper.ToString: string;
begin
  Result := VkAttachmentType[Self];
end;

{ TVkPeerTypeHelper }

class function TVkPeerTypeHelper.Create(Value: string): TVkPeerType;
begin
  Result := TVkPeerType(IndexStr(Value, VkPeerType));
end;

function TVkPeerTypeHelper.ToString: string;
begin
  Result := VkPeerType[Self];
end;

{ TVkPostTypeHelper }

function TVkPostTypeHelper.ToString: string;
begin
  Result := VKPostType[Self];
end;

{ TVkPermissionHelper }

function TVkPermissionHelper.ToString: string;
begin
  Result := VkPremissionStr[Self];
end;

{ TVkPermissionsHelper }

procedure TVkPermissionsHelper.Include(Value: TVkPermission);
begin
  System.Include(Self, Value);
end;

function TVkPermissionsHelper.ToString: string;
var
  Item: TVkPermission;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupFieldHelper }

function TVkGroupFieldHelper.ToString: string;
begin
  Result := VkGroupField[Self];
end;

{ TVkGroupFieldsHelper }

class function TVkGroupFieldsHelper.All: TVkGroupFields;
begin
  Result := [TVkGroupField.City, TVkGroupField.Country, TVkGroupField.Place,
    TVkGroupField.Description, TVkGroupField.WikiPage, TVkGroupField.MembersCount,
    TVkGroupField.Counters, TVkGroupField.StartDate, TVkGroupField.FinishDate,
    TVkGroupField.CanPost, TVkGroupField.CanSeeAllPosts, TVkGroupField.Activity,
    TVkGroupField.Status, TVkGroupField.Contacts, TVkGroupField.Links,
    TVkGroupField.FixedPost, TVkGroupField.Verified, TVkGroupField.Site,
    TVkGroupField.CanCreateTopic, TVkGroupField.Photo50];
end;

function TVkGroupFieldsHelper.ToString: string;
var
  Item: TVkGroupField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkProfileFieldsHelper }

class function TVkProfileFieldsHelper.All: TVkProfileFields;
begin
  Result := [TVkProfileField.PhotoId, TVkProfileField.Verified, TVkProfileField.Sex,
    TVkProfileField.BirthDate, TVkProfileField.City, TVkProfileField.Country,
    TVkProfileField.HomeTown, TVkProfileField.HasPhoto, TVkProfileField.Photo50,
    TVkProfileField.Photo100, TVkProfileField.Photo200Orig, TVkProfileField.Photo200,
    TVkProfileField.Photo400Orig, TVkProfileField.PhotoMax, TVkProfileField.PhotoMaxOrig,
    TVkProfileField.Online, TVkProfileField.Domain, TVkProfileField.HasMobile,
    TVkProfileField.Contacts, TVkProfileField.Site, TVkProfileField.Education,
    TVkProfileField.Universities, TVkProfileField.Schools, TVkProfileField.Status,
    TVkProfileField.LastSeen, TVkProfileField.FollowersCount, TVkProfileField.CommonCount,
    TVkProfileField.Occupation, TVkProfileField.Nickname, TVkProfileField.Relatives,
    TVkProfileField.Relation, TVkProfileField.Personal, TVkProfileField.Connections,
    TVkProfileField.&Exports, TVkProfileField.Activities, TVkProfileField.Interests,
    TVkProfileField.Music, TVkProfileField.Movies, TVkProfileField.TV, TVkProfileField.Books,
    TVkProfileField.Games, TVkProfileField.About, TVkProfileField.Quotes,
    TVkProfileField.CanPost, TVkProfileField.CanSeeAllPosts, TVkProfileField.CanSeeAudio,
    TVkProfileField.CanWritePrivateMessage, TVkProfileField.CanSendFriendRequest,
    TVkProfileField.IsFavorite, TVkProfileField.IsHiddenFromFeed, TVkProfileField.TimeZone,
    TVkProfileField.ScreenName, TVkProfileField.MaidenName, TVkProfileField.CropPhoto,
    TVkProfileField.IsFriend, TVkProfileField.FriendStatus, TVkProfileField.Career,
    TVkProfileField.Military, TVkProfileField.Blacklisted, TVkProfileField.BlacklistedByMe,
    TVkProfileField.CanBeInvitedGroup];
end;

function TVkProfileFieldsHelper.ToString: string;
var
  Item: TVkProfileField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkProfileFieldHelper }

function TVkProfileFieldHelper.ToString: string;
begin
  Result := VkProfileField[Self];
end;

{ TVkGroupFilterHelper }

function TVkGroupFilterHelper.ToString: string;
begin
  Result := VkGroupFilter[Self];
end;

{ TVkGroupFiltersHelper }

class function TVkGroupFiltersHelper.All: TVkGroupFilters;
begin
  Result := [TVkGroupFilter.Admin, TVkGroupFilter.Editor, TVkGroupFilter.Moder,
    TVkGroupFilter.Advertiser, TVkGroupFilter.Groups, TVkGroupFilter.Publics,
    TVkGroupFilter.Events, TVkGroupFilter.HasAddress];
end;

function TVkGroupFiltersHelper.ToString: string;
var
  Item: TVkGroupFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupMembersFilterHelper }

function TVkGroupMembersFilterHelper.ToString: string;
begin
  Result := VkGroupMembersFilter[Self];
end;

{ TVkSortIdTimeHelper }

function TVkSortIdTimeHelper.ToString: string;
begin
  Result := VkSortIdTime[Self];
end;

{ TVkCounterFilterHelper }

function TVkCounterFilterHelper.ToString: string;
begin
  Result := VkCounterFilter[Self];
end;

{ TVkCounterFiltersHelper }

function TVkCounterFiltersHelper.ToString: string;
var
  Item: TVkCounterFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkInfoFilterHelper }

function TVkInfoFilterHelper.ToString: string;
begin
  Result := VkInfoFilter[Self];
end;

{ TVkInfoFiltersHelper }

function TVkInfoFiltersHelper.ToString: string;
var
  Item: TVkInfoFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupTypeHelper }

class function TVkGroupTypeCreateHelper.Create(const Value: string): TVkGroupTypeCreate;
begin
  Result := TVkGroupTypeCreate(IndexStr(Value, VkGroupTypeCreate));
end;

function TVkGroupTypeCreateHelper.ToString: string;
begin
  Result := VkGroupTypeCreate[Self];
end;

{ TVkGroupTypeHelper }

class function TVkGroupTypeHelper.Create(const Value: string): TVkGroupType;
begin
  Result := TVkGroupType(IndexStr(Value, VkGroupType));
end;

function TVkGroupTypeHelper.ToString: string;
begin
  Result := VkGroupType[Self];
end;

{ TVkMarketCurrencyHelper }

function TVkMarketCurrencyHelper.ToConst: Integer;
begin
  Result := VkCurrencyId[Self];
end;

{ TVkGroupRoleHelper }

function TVkGroupRoleHelper.ToString: string;
begin
  Result := VkGroupRole[Self];
end;

{ TVkGroupAddressFieldHelper }

function TVkGroupAddressFieldHelper.ToString: string;
begin
  Result := VkGroupAddressField[Self];
end;

{ TVkGroupAddressFieldsHelper }

class function TVkGroupAddressFieldsHelper.All: TVkGroupAddressFields;
begin
  Result := [TVkGroupAddressField.Title, TVkGroupAddressField.Address,
    TVkGroupAddressField.AdditionalAddress, TVkGroupAddressField.CountryId,
    TVkGroupAddressField.CityId, TVkGroupAddressField.MetroStationId,
    TVkGroupAddressField.Latitude, TVkGroupAddressField.Longitude,
    TVkGroupAddressField.WorkInfoStatus, TVkGroupAddressField.TimeOffset];
end;

function TVkGroupAddressFieldsHelper.ToString: string;
var
  Item: TVkGroupAddressField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkSortHelper }

function TVkSortHelper.ToString: string;
begin
  Result := VkSort[Self];
end;

{ TVkPhotoReportReasonHelper }

function TVkMediaReportReasonHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

function TVkMediaReportReasonHelper.ToString: string;
begin
  Result := VkPhotoReportReason[Self];
end;

{ TVkMethodException }

constructor TVkMethodException.Create(const Msg: string; Code: Integer);
begin
  inherited Create(Msg);
  FCode := Code;
end;

procedure TVkMethodException.SetCode(const Value: Integer);
begin
  FCode := Value;
end;

{ TVkUserReportHelper }

function TVkUserReportHelper.ToString: string;
begin
  Result := VkUserReport[Self];
end;

{ TVkPostLinkButtonHelper }

function TVkPostLinkButtonHelper.ToString: string;
begin
  Result := VKPostLinkButton[Self];
end;

{ TVkLinkStatusTypeHelper }

class function TVkLinkStatusTypeHelper.FromString(Value: string): TVkLinkStatusType;
begin
  Result := TVkLinkStatusType(IndexStr(Value, VkLinkStatusType));
end;

function TVkLinkStatusTypeHelper.ToString: string;
begin
  Result := VkLinkStatusType[Self];
end;

{ TVkValidationTypeHelper }

class function TVkValidationTypeHelper.FromString(const Value: string): TVkValidationType;
begin
  Result := TVkValidationType(IndexStr(Value, VkValidationType));
end;

{ TMessageFlagHelper }

function TVkMessageFlagHelper.ToString: string;
begin
  Result := VkMessageFlagTypes[Self];
end;

{ TVkMessageActionTypeHelper }

class function TVkMessageActionTypeHelper.Create(const Value: string): TVkMessageActionType;
begin
  Result := TVkMessageActionType(IndexStr(Value, VkMessageActionType));
end;

function TVkMessageActionTypeHelper.ToString: string;
begin
  Result := VkMessageActionType[Self];
end;

{ TVkPlatformHelper }

class function TVkPlatformHelper.Create(const Value: string): TVkPlatform;
begin
  Result := TVkPlatform(IndexStr(Value, VkPlatformsType));
end;

function TVkPlatformHelper.ToString: string;
begin
  Result := VkPlatformsType[Self];
end;

{ TVkNameRequestStatusHelper }

class function TVkNameRequestStatusHelper.Create(const Value: string): TVkNameRequestStatus;
begin
  Result := TVkNameRequestStatus(IndexStr(Value, VkNameRequestStatus));
end;

function TVkNameRequestStatusHelper.ToString: string;
begin
  Result := VkNameRequestStatus[Self];
end;

{ TVkDeactivatedHelper }

class function TVkDeactivatedHelper.Create(const Value: string): TVkDeactivated;
begin
  Result := TVkDeactivated(IndexStr(Value, VkDeactivated));
end;

function TVkDeactivatedHelper.ToString: string;
begin
  Result := VkDeactivated[Self];
end;

{ TVkKeyboardButtonColorHelper }

class function TVkKeyboardButtonColorHelper.Create(Value: string): TVkKeyboardButtonColor;
begin
  Result := TVkKeyboardButtonColor(IndexStr(Value, VkKeyboardButtonColor));
end;

function TVkKeyboardButtonColorHelper.ToColor: TAlphaColor;
begin
  Result := VkKeyboardButtonColorValue[Self];
end;

function TVkKeyboardButtonColorHelper.ToString: string;
begin
  Result := VkKeyboardButtonColor[Self];
end;

{ TAttachment }

class function TAttachment.Create(&Type: string; OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result.&Type := &Type;
  Result.OwnerId := OwnerId;
  Result.Id := Id;
  Result.AccessKey := AccessKey;
end;

class function TAttachment.Audio(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('audio', OwnerId, Id, AccessKey);
end;

class function TAttachment.Doc(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('doc', OwnerId, Id, AccessKey);
end;

class function TAttachment.Gift(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('gift', OwnerId, Id, AccessKey);
end;

class function TAttachment.Link(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('link', OwnerId, Id, AccessKey);
end;

class function TAttachment.Market(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('market', OwnerId, Id, AccessKey);
end;

class function TAttachment.MarketAlbum(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('market_album', OwnerId, Id, AccessKey);
end;

class function TAttachment.Photo(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('photo', OwnerId, Id, AccessKey);
end;

class function TAttachment.Sticker(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('sticker', OwnerId, Id, AccessKey);
end;

class function TAttachment.Video(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('video', OwnerId, Id, AccessKey);
end;

class function TAttachment.Wall(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('wall', OwnerId, Id, AccessKey);
end;

class function TAttachment.WallReply(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('wall_reply', OwnerId, Id, AccessKey);
end;

class function TAttachment.Album(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create('album', OwnerId, Id, AccessKey);
end;

function TAttachment.ToString: string;
begin
  Result := &Type + OwnerId.ToString + '_' + Id.ToString;
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

class operator TAttachment.Implicit(const Value: TAttachment): string;
begin
  Result := Value.ToString;
end;

class operator TAttachment.Implicit(const Value: string): TAttachment;
var
  i: Integer;
  tmp, tmpValue: string;
begin
  //video58553419_456239240_wefq76wegq7we
  tmpValue := Value + '_';
  tmp := '';
  Result.OwnerId := 0;
  Result.Id := 0;
  Result.AccessKey := '';
  Result.&Type := '';
  for i := 0 to Pred(tmpValue.Length) do
  begin
    if Result.&Type.IsEmpty then
    begin
      if tmpValue.Chars[i].IsDigit then
      begin
        Result.&Type := tmp;
        tmp := '';
      end;
    end
    else if Result.OwnerId = 0 then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.OwnerId := StrToIntDef(tmp, 0);
        tmp := '';
        Continue;
      end;
    end
    else if Result.Id = 0 then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.Id := StrToIntDef(tmp, 0);
        tmp := '';
        Continue;
      end;
    end
    else if Result.AccessKey.IsEmpty then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.AccessKey := tmp;
        tmp := '';
        Continue;
      end;
    end;
    tmp := tmp + tmpValue.Chars[i];
  end;
end;

{ TAttachmentArrayHelper }

function TAttachmentArrayHelper.IsEmpty: Boolean;
begin
  Result := Length(Self) = 0;
end;

function TAttachmentArrayHelper.ToStrings: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Length(Self));
  for i := Low(Self) to High(Self) do
    Result[i] := Self[i];
end;

{ TVkGroupStatusTypeHelper }

class function TVkGroupStatusTypeHelper.Create(const Value: string): TVkGroupStatusType;
begin
  Result := TVkGroupStatusType(IndexStr(Value, VkGroupStatusType));
end;

function TVkGroupStatusTypeHelper.ToString: string;
begin
  Result := VkGroupStatusType[Self];
end;

end.

